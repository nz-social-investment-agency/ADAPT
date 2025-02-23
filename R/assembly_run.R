################################################################################
#' Notes
#' - documentation detail bullet points to do
#' 
################################################################################

#' Execute assembly tool.
#' 
#' @param control_file a data frame containing assembly instructions. Most
#' likely read into memory by `load_control_file`.
#' @param db_connection A connection to the database where assembly is to occur.
#' @param master_table The name of the table onto which columns should be
#' assembled. This table must already exist in the database. It is recommended
#' using the full table name: database.schema.table
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
#' *
#' 
#' Anywhere dynamic input is accepted in the control file, you can instead give
#' input in \{curry brackets\}. This input is treated as SQL code.
#' 
#' The intended use of this feature is for making minor adjustments to
#' variables. For example setting zero values to missing or adjusting dates.
#' @md
#' 
#' @importFrom  rlang .data
#' @export
run_assembly = function(control_file, db_connection, master_table, debug_folder = NA_character_){
  stopifnot(is.data.frame(control_file))
  stopifnot(DBI::dbIsValid(db_connection))
  stopifnot(is.character(master_table))
  stopifnot(is.character(debug_folder))
  stopifnot(is.na(debug_folder) | dir.exists(debug_folder) )

  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  
  valid_control_file = validate_assembly_control_file(control_file, db_connection, master_table)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  if(nrow(control_file) == 0){
    warning("All rows of control file disabled, returnig NULL")
    return(NULL)
  }
  
  # sqlite flag
  is_sqlite = any(grepl("sqlite", class(db_connection), ignore.case = TRUE))
  
  ## setup for assembly ----
  
  # handle entity types
  control_file = entity_to_min_and_max(control_file)
  
  # distinct combinations
  distinction_cols = c("population_uid", "period_start", "period_end",
                       "measure_table", "measure_uid", "measure_start", "measure_end")
  summary_combinations = dplyr::select(control_file, dplyr::all_of(distinction_cols))
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
  
  ## assembly ----
  
  for(rr in seq_len(nrow(summary_combinations))){
    ### setup ----
    
    # extract
    this_row = summary_combinations[rr, ,drop = FALSE]
    summary_rows = dplyr::semi_join(control_file, this_row, by = colnames(this_row))
    
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
        row = summary_rows[rnum, , drop = FALSE]
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
        row = summary_rows[rnum, , drop = FALSE]
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
    DBI::dbExecute(db_connection, prepared_query)
  }

  ## conclude ----
  return(invisible(1))
}
