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
  
  ## setup for assembly ----
  
  # distinct combinations
  distinction_cols = c("population_uid", "period_start", "period_end",
                       "measure_table", "measure_uid", "measure_start", "measure_end")
  summary_combinations = dplyr::select(control_file, dplyr::all_of(distinction_cols))
  summary_combinations = dplyr::distinct(summary_combinations)
  
  # master table
  remote_master_table = dplyr::tbl(db_connection, I(master_table))
  master_columns = colnames(remote_master_table)
  
  # core query
  query_text = c(
    "WITH distinct_mt AS (\n",
    "    SELECT DISTINCT {this_row$population_uid}\n",
    "        , {this_row$period_start}\n",
    "        , {this_row$period_end}\n",
    "    FROM {master_table}\n",
    "),\n",
    "setup AS (\n",
    "    SELECT dmt.{this_row$population_uid}\n",
    "        , dmt.{this_row$period_start}\n",
    "        , dmt.{this_row$period_end}\n",
    "        , {update_summary_list}\n",
    "    FROM distinct_mt AS dmt\n",
    "    INNER JOIN {this_row$measure_table} AS m\n",
    "    ON dmt.{this_row$population_uid} = m.{this_row$measure_uid}\n",
    "    AND dmt.{this_row$period_start} <= m.{this_row$measure_end}\n",
    "    AND dmt.{this_row$measure_start} <= m.{this_row$period_end}\n",
    "    GROUP BY dmt.{this_row$population_uid}\n",
    "        , dmt.{this_row$period_start}\n",
    "        , dmt.{this_row$period_end}\n",
    ")\n",
    "UPDATE mt\n",
    "SET {update_set_col_list}\n",
    "FROM {master_table} AS mt\n",
    "INNER JOIN setup AS s\n",
    "ON mt.{this_row$population_uid} = s.{this_row$population_uid}\n",
    "AND mt.{this_row$period_start} = s.{this_row$period_start}\n",
    "AND mt.{this_row$period_end} = s.{this_row$period_end}"
  )
  
  ## assembly ----
  
  for(rr in seq_len(nrow(summary_combinations))){
    ### setup ----
    
    # extract
    this_row = summary_combinations[rr, ,drop = FALSE]
    summary_rows = dplyr::semi_join(control_file, this_row, by = colnames(this_row))
    
    # delimiter conversion
    cols_to_convert = c("period_start", "period_end", "measure_start", "measure_end", "measure_value")
    for(cc in cols_to_convert){
      to_single_quote = is_delimited(summary_rows[[cc]], "\"")
      summary_rows[[cc]][to_single_quote] = remove_delimiters(summary_rows[[cc]][to_single_quote], "\"")
      summary_rows[[cc]][to_single_quote] = add_delimiters(summary_rows[[cc]][to_single_quote], "'")
      
      to_unquoted = is_delimited(summary_rows[[cc]], "{}")
      summary_rows[[cc]][to_unquoted] = trimws(remove_delimiters(summary_rows[[cc]][to_unquoted], "{}"))
    }
    
    ### columns dropped and added ----
    
    output_col_names = remove_delimiters(summary_rows$output_name, "\"")
    
    # drop columns
    query = alter_table_drop_column(master_table, intersect(colnames(master_columns), output_col_names))
    if(!is.na(debug_folder)){
      save_code_to_script(query, "drop columns.sql", debug_folder)
    }
    DBI::dbExecute(db_connection, query)
    
    # create columns
    query = alter_table_add_column(master_table, output_col_names, summary_rows$output_type)
    if(!is.na(debug_folder)){
      save_code_to_script(query, "create columns.sql", debug_folder)
    }
    DBI::dbExecute(db_connection, query)
    
    ### prepare query ----

    # summary columns list
    update_summary_list = lapply(
      seq_len(nrow(summary_rows)),
      function(rnum, is_sqlite){
        row = summary_rows[rnum, , drop = FALSE]
        handle_summary_case(row, is_sqlite)
      },
      is_sqlite = any(grepl("sqlite", class(db_connection), ignore.case = TRUE))
    )
    update_summary_list = unlist(update_summary_list, use.names = FALSE)
    update_summary_list = paste(update_summary_list, collapse = "\n    , ")
    
    # set column list for update
    update_set_col_list = lapply(
      seq_len(nrow(summary_rows)),
      function(rnum){
        row = summary_rows[rnum, , drop = FALSE]
        suffix = ""
        suffix = if(row$output_method == "ENTITY"){ suffix = c("__min", "__max") }
        glue::glue("mt.{row$output_name}{suffix} = s.{row$output_name}{suffix}")
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
