#' Execute assembly tool.
#' 
#' @param control_file location of the control file containing assembly
#' instructions to read into R. Accepted `.csv` and `.xlsx` file formats.
#' @param sheet Sheet to read if control file is `.xlsx` format. As per
#' `openxlsx2::read_xlsx`: either a string (name of a sheet), or an integer
#' (the position of the sheet). Defaults to the first sheet otherwise.
#' @param db_connection A connection to the database where assembly is to occur.
#' @param master_table The name of the table onto which columns should be
#' assembled. This table must already exist in the database. It is recommended
#' using the full table name: database.schema.table
#' @param sql_folder Optional folder location containing SQL scripts. If given
#' and tables listed in the control file can not be found in the database, then
#' will check for evidence of table and column names in this folder.
#' @param debug_folder an existing folder where debug information should be
#' written to disc. If NA (the default) not debug information is written.
#' 
#' @return None, makes permanent changes to `master_table` in the database.
#'
#' @details
#' The best way to understand the assembly process is to review a worked example.
#' Try `provide_example` for worked examples, or for example control files.
#' 
#' For each row in the control file, the assembly tool appends a column to
#' the master table according to the specified instructions. If the column
#' already exists it is overwritten.
#' 
#' Control files are validated prior to execution. To validate a control file
#' without execution use `validate_assembly_control_file`.
#' 
#' The accepted columns for the control file are:
#' * Enabled - Optional but recommended column, containing True / False or
#' yes / no. Allows for rows of the control file to be turned on and off.
#' * Description  - Optional but recommended column, for user readable
#' description of the intent of the output column. Not used during assembly.
#' * Population_uid  - The name of the identity/uid column in the master table
#' to use for assembly, delimited with [].
#' * Period_start  - The start of the period, can be a column of the master
#' table, a constant, or SQL code that generates the data.
#' * Period_end  - The end of the period, can be a column of the master table,
#' a constant, or SQL code that generates the data.
#' * Measure_file  - The name of the SQL file that prepares the measure. Useful
#' for tracking the lineage of a measure. If the measure table or columns are
#' not found in the database, then the file is checked during validation.
#' * Measure_table  - The name of the SQL table from which the measure is drawn.
#' * Measure_uid  - The name of the identity/uid column in the measure table,
#' delimited with [].
#' * Measure_start  - The start of the measure event, can be a column of the
#' measure table, a constant, or SQL code that generates the data.
#' * Measure_end  - The end of the measure event, can be a column of the measure
#' table, a constant, or SQL code that generates the data.
#' * Measure_value  - The value that should be summarised to create a column in
#' the master table, can be a column of the measure table, a constant, or SQL
#' code that generates the data.
#' * Output_name  - The name of the output column, should be unique and
#' delimited with "". For ease of subsequent use it is best to use underscore
#' instead of spaces in these names.
#' * Output_method  - How the measure should be summarised to create the column
#' for the master table.
#' * Output_type  - The SQL data type for the new column.
#' * Notes - free text column for adding notes to the control file. This column
#' is ignored during assembly and does not effect output. Any other column names
#' are also ignored, but generate a warning.
#' 
#' For all inputs, SQL objects (like table and column names) should be delimited
#' with `[]`; constants should be delimited with `""`, and dynamic input should
#' be delimited with `{}`. Dynamic input is treated as SQL code.
#' 
#' The intended use of this feature is for making minor adjustments to
#' variables. For example setting zero values to missing or adjusting dates.
#' @md
#' 
#' @importFrom  rlang .data
#' @export
run_assembly = function(control_file, sheet = NULL, db_connection, master_table, sql_folder = NA_character_, debug_folder = NA_character_){
  stopifnot(is.character(control_file), file.exists(control_file))
  stopifnot(is.null(sheet) | is.character(sheet))
  stopifnot(DBI::dbIsValid(db_connection))
  stopifnot(is.character(master_table))
  stopifnot(is.character(debug_folder))
  stopifnot(is.na(debug_folder) | dir.exists(debug_folder) )
  
  run_time_inform_user("Assembly tool initiated.")
  
  ## load control file ----
  
  loaded_cf = load_control_file(control_file, sheet = sheet)
  # drop progress reporting columns
  loaded_cf = dplyr::select(loaded_cf, -dplyr::any_of(c("start_time", "end_time", "status")))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(loaded_cf)))
  colnames(loaded_cf) = ctr_cols
  
  valid_control_file = validate_assembly_control_file(loaded_cf, db_connection, master_table, sql_folder = sql_folder)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    loaded_cf = dplyr::filter(loaded_cf, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  if(nrow(loaded_cf) == 0){
    warning("All rows of control file disabled, returning NULL")
    return(NULL)
  }
  
  # sqlite flag
  is_sqlite = any(grepl("sqlite", class(db_connection), ignore.case = TRUE))
  
  ## setup for assembly ----
  
  # handle entity types
  loaded_cf = entity_to_min_and_max(loaded_cf)
  
  # distinct combinations
  distinction_cols = c("population_uid", "period_start", "period_end",
                       "measure_table", "measure_uid", "measure_start", "measure_end")
  summary_combinations = dplyr::select(loaded_cf, dplyr::all_of(distinction_cols))
  summary_combinations = dplyr::distinct(summary_combinations)
  
  # master table
  remote_master_table = dplyr::tbl(db_connection, I(master_table))
  
  # core query
  update_clause = ifelse(
    is_sqlite,
    # SQLite syntax
    paste0(
      "UPDATE {master_table} AS mt\n",
      "SET {update_set_col_list}\n",
      "FROM  setup\n",
      "WHERE {this_row$population_uid} = setup.core_query_p_uid\n",
      "AND {this_row$period_start} = setup.core_query_p_start\n",
      "AND {this_row$period_end} = setup.core_query_p_end"
    ),
    # SQL Server syntax
    paste0(
      "UPDATE mt\n",
      "SET {update_set_col_list}\n",
      "FROM {master_table} AS mt\n",
      "INNER JOIN setup AS s\n",
      "ON {this_row$population_uid} = s.core_query_p_uid\n",
      "AND {this_row$period_start} = s.core_query_p_start\n",
      "AND {this_row$period_end} = s.core_query_p_end"
    )
  )
  
  query_text = paste0(
    "WITH distinct_mt AS (\n",
    "    SELECT DISTINCT {this_row$population_uid} AS core_query_p_uid\n",
    "        , {this_row$period_start} AS core_query_p_start\n",
    "        , {this_row$period_end} AS core_query_p_end\n",
    "    FROM {master_table} AS mt\n",
    "),\n",
    "setup AS (\n",
    "    SELECT dmt.core_query_p_uid\n",
    "        , dmt.core_query_p_start\n",
    "        , dmt.core_query_p_end\n",
    "        , {update_summary_list}\n",
    "    FROM distinct_mt AS dmt\n",
    "    INNER JOIN {this_row$measure_table} AS m\n",
    "    ON dmt.core_query_p_uid = m.{this_row$measure_uid}\n",
    "    AND dmt.core_query_p_start <= {this_row$measure_end}\n",
    "    AND {this_row$measure_start} <= dmt.core_query_p_end\n",
    "    GROUP BY dmt.core_query_p_uid\n",
    "        , dmt.core_query_p_start\n",
    "        , dmt.core_query_p_end\n",
    ")\n",
    update_clause
  )
  
  ## results ----
  
  result_df = dplyr::mutate(
    summary_combinations,
    start_time = NA_character_,
    end_time = NA_character_,
    status = NA_character_
  )
  on.exit(expr = {
    save_control_file_w_progress(control_file, sheet = sheet, result_df)
  }, add = TRUE, after = TRUE)
  
  ## assembly ----
  
  for(rr in seq_len(nrow(summary_combinations))){
    ### setup ----
    
    # extract
    this_row = dplyr::slice(summary_combinations, rr)
    summary_rows = dplyr::semi_join(loaded_cf, this_row, by = colnames(this_row))
    
    remote_measure_table = dplyr::tbl(db_connection, I(this_row$measure_table))
    
    # assign alias prefixes
    this_row = handle_delimiters_and_prefixes(
      this_row,
      mt_prefix = "mt",
      mt_cols = colnames(remote_master_table),
      measure_prefix = "m",
      measure_cols = colnames(remote_measure_table)
    )
    summary_rows = handle_delimiters_and_prefixes(
      summary_rows,
      mt_prefix = "dmt",
      mt_cols = colnames(remote_master_table),
      measure_prefix = "m",
      measure_cols = colnames(remote_measure_table)
    )
    
    ### columns dropped and added ----
    
    output_col_names = remove_delimiters(summary_rows$output_name, "\"")
    
    # drop columns
    query = alter_table_drop_column(master_table, intersect(colnames(remote_master_table), output_col_names), is_sqlite)
    if(!is.na(debug_folder)){
      save_code_to_script(query, "drop columns.sql", debug_folder)
    }
    sapply(query, DBI::dbExecute, conn = db_connection)

    # create columns
    query = alter_table_add_column(master_table, output_col_names, summary_rows$output_type, is_sqlite)
    if(!is.na(debug_folder)){
      save_code_to_script(query, "create columns.sql", debug_folder)
    }
    sapply(query, DBI::dbExecute, conn = db_connection)

    ### prepare query ----

    # summary columns list
    update_summary_list = lapply(
      seq_len(nrow(summary_rows)),
      function(rnum, is_sqlite){
        row = dplyr::slice(summary_rows, rnum)
        handle_summary_case(row, sqlite = is_sqlite)
      },
      is_sqlite = is_sqlite
    )
    update_summary_list = unlist(update_summary_list, use.names = FALSE)
    update_summary_list = paste(update_summary_list, collapse = "\n        , ")
    
    # set column list for update
    update_set_col_list = lapply(
      seq_len(nrow(summary_rows)),
      function(rnum){
        row = dplyr::slice(summary_rows, rnum)
        prefix1 = ifelse(is_sqlite, "", "mt.")
        prefix2 = ifelse(is_sqlite, "setup.", "s.")
        glue::glue("{prefix1}{row$output_name} = {prefix2}{row$output_name}")
      }
    )
    update_set_col_list = unlist(update_set_col_list, use.names = FALSE)
    update_set_col_list = paste(update_set_col_list, collapse = "\n    , ")
    
    # combine whole query
    prepared_query = glue::glue(query_text)
    
    ### save query if debug ----
    if(!is.na(debug_folder)){
      save_code_to_script(prepared_query, "assembly.sql", debug_folder)
    }

    ### execute query ----
    
    msg = sprintf("Assembly step %3d of %d: Started", rr, nrow(summary_combinations))
    run_time_inform_user(msg)
    result = try_run_SQL_query(prepared_query, db_connection)
    
    ## log results ----
    
    msg = sprintf("Assembly step %3d of %d: %s", rr, nrow(summary_combinations), result$status)
    run_time_inform_user(msg)
    
    result_df$start_time[rr] = result$start_time
    result_df$end_time[rr] = result$end_time
    result_df$status[rr] = result$status
  }

  ## conclude ----
  run_time_inform_user("Assembly tool complete.")
  return(invisible(result_df))
}

## Try run assembly query ------------------------------------------------- ----
#' Execute SQL query with error handling
#'
#' @param query The query to execute
#' @param db_connection A connection to the database where assembly is to occur.
#' @param ignore_warnings T/F whether execution should continue even if warnings
#' occur. If `FALSE` (the default) then will stop on the first warning and
#' return the warning message. If `TRUE` then will suppress all warnings.
#'
#' @return A list containing three components:
#' * status - Status message, of success or stopped with error/warning and the
#' error/warning message.
#' * start_time - the system time when the query started running.
#' * end_time - the system time when the query ceased running.
#' @md
#' 
#' @details
#' The SQL query is executed in the database environment on the provided
#' connection.
#' 
#' It is assumed that `query` is a single SQL query. This is not validated and
#' multi-query submissions may work, but are not supported. The internal
#' function `try_run_SQL_file` is designed to handle entire SQL scripts and may
#' be suitable for multi-query applications.
#' 
#' @export
try_run_SQL_query = function(query, db_connection, ignore_warnings = FALSE){
  stopifnot(is.character(query))
  stopifnot(DBI::dbIsValid(db_connection))
  stopifnot(ignore_warnings %in% c(TRUE, FALSE))
  
  start_time = as.character(Sys.time())
  
  # execute, capturing messages
  status = tryCatch(
    {
      if(ignore_warnings){
        result = suppressWarnings(DBI::dbExecute(db_connection, query, immediate = TRUE))
      } else {
        DBI::dbExecute(db_connection, query, immediate = TRUE)
      }
      "Successful completion"
    },
    error = function(e){
      msg = paste(e$message, collapse = "\n")
      msg = glue::glue("Stopped with error: ", msg)
      return(msg)
    },
    warning = function(w){
      msg = paste(w$message, collapse = "\n")
      msg = paste("Stopped with warning: ", msg)
      return(msg)
    }
  )
  
  # conclude
  end_time = as.character(Sys.time())
  return(list(status = status, start_time = start_time, end_time = end_time))
}
