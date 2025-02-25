#' Confirm that control file instructions can be executed.
#' 
#' @param control_file a data frame containing assembly instructions. Most
#' likely read into memory by `load_control_file`.
#' @param db_connection A connection to the database where assembly is to occur.
#' @param master_table The name of the table onto which columns should be
#' assembled. It is recommended using the full table name: database.schema.table
#' @param sql_folder Optional folder location containing SQL scripts. If given
#' and tables listed in the control file can not be found in the database, then
#' will check for evidence of table and column names in this folder.
#'
#' @return T/F whether or not all validation checks are passed. Generating
#' warnings for all failed checks.
#'
#' @details
#' The following checks are run and generate a failure if not passed:
#' * Required column names are present.
#' * Inputs match expected formats / delimiters.
#' * Dynamic inputs contain `no_obvious_escaping_injection`
#' * Master table exists.
#' * Master table columns exist.
#' * Measure tables exist in the database or are found in the measure_file.
#' * Measure columns exist in the database or are found in the measure_file.
#' * Output methods are accepted types.
#' * Output types are accepted SQL data types
#' 
#' The following checks are run and only generate a warning if not passed:
#' * Unrecognised column names are present.
#' 
#' @md
#' 
#' @importFrom  rlang .data
#' @export
validate_assembly_control_file = function(control_file, db_connection, master_table, sql_folder = NA_character_){
  stopifnot(is.data.frame(control_file))
  stopifnot(DBI::dbIsValid(db_connection))
  stopifnot(is.character(master_table))
  stopifnot(is.character(sql_folder))
  stopifnot(is.na(sql_folder) || dir.exists(sql_folder))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  ## setup for checks ----
  
  # acceptable and default values
  column_reqs = data.frame(
    row.names = c("[sql]", "\"text\"", "{dynamic}"),
    population_uid = c(TRUE, FALSE, FALSE),
    period_start = c(TRUE, TRUE, TRUE),
    period_end = c(TRUE, TRUE, TRUE),
    measure_uid = c(TRUE, FALSE, FALSE),
    measure_start = c(TRUE, TRUE, TRUE),
    measure_end = c(TRUE, TRUE, TRUE),
    measure_value = c(TRUE, TRUE, TRUE),
    output_name = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  # track passing of checks
  passes_all_critical_checks = TRUE
  
  ## Required column names are present ----
  
  req_cols = c("population_uid", "period_start", "period_end", 
               "measure_table", "measure_uid", "measure_start", "measure_end",
               "measure_value", "output_name", "output_method", "output_type")
  missing_cols = setdiff(req_cols, colnames(control_file))
  
  for(col in missing_cols){
    msg = glue::glue("Control file is missing required column '{col}'.")
    warning(msg)
  }
  
  if(length(missing_cols) > 0){
    return(FALSE)
  }
  
  ## Unrecognised column names are present (no error) ----
  
  optional_cols = c("enabled", "description", "measure_file", "note", "notes")
  extra_cols = setdiff(colnames(control_file), c(req_cols, optional_cols))
  
  for(col in extra_cols){
    msg = glue::glue("Unrecognised control file column '{col}' ignored.")
    warning(msg)
  }
  
  ## Inputs match expected formats / delimiters ----
  
  # for every column and row
  for (col in colnames(column_reqs)) {
    
    accepts_sql = as.logical(column_reqs["[sql]", col])
    accepts_txt = as.logical(column_reqs["\"text\"", col])
    accepts_dyn = as.logical(column_reqs["{dynamic}", col])
    
    for (row in seq_len(nrow(control_file))) {
      
      this_value = control_file[[row, col]]
      
      # missing
      if(is.na(this_value)){
        passes_all_critical_checks = FALSE
        msg = glue::glue("Missing input in column '{col}', row '{row}'")
        warning(msg)
        next
      }
      
      # correct deliminators
      delim_sql = is_delimited(this_value, "[]")
      delim_txt = is_delimited(this_value, "\"")
      delim_dyn = is_delimited(this_value, "{}")
      
      pass = (accepts_sql && delim_sql) || (accepts_txt && delim_txt) || (accepts_dyn && delim_dyn)
      
      if(!pass){
        passes_all_critical_checks = FALSE
        accepted = paste(rownames(column_reqs)[column_reqs[[col]]], collapse = ", ")
        msg = glue::glue("Unaccepted input in column '{col}', row '{row}'\n",
                         "Accepted values are {accepted}")
        warning(msg)
      }
    } # end row iteration
  } # end column iteration
  
  ## Dynamic inputs contain on escaping code injection ----
  
  # get only dynamic cells in control file
  dynamic_cells = unlist(control_file, use.names = FALSE)
  dynamic_cells = dynamic_cells[!is.na(dynamic_cells)]
  dynamic_cells = dynamic_cells[is_delimited(dynamic_cells, "{}")]
  dynamic_cells = unique(dynamic_cells)
  
  dynamic_cells = trimws(dynamic_cells)
  dynamic_cells = remove_delimiters(dynamic_cells, "{}")
  
  for(cell in dynamic_cells){
    if(no_obvious_escaping_injection(cell)){ next }
    msg = glue::glue("Dynamic input '{cell}' rejected due to potential escaping code injection")
    warning(msg)
    passes_all_critical_checks = FALSE
  }

  ## Master table exists ----
  
  master_table_exists = DBI::dbExistsTable(db_connection, sql2id(master_table))
  
  if(!master_table_exists){
    msg = glue::glue("Master table '{master_table}' not found in database.")
    warning(msg)
  }
  
  passes_all_critical_checks = passes_all_critical_checks & master_table_exists
  
  ## Master table columns exist ----
  
  if(master_table_exists){
    # columns to check
    cols_to_check = c(control_file$population_uid, control_file$period_start, control_file$period_end)
    cols_to_check = cols_to_check[is_delimited(cols_to_check, "[]")]
    cols_to_check = unique(cols_to_check)
    cols_to_check = remove_delimiters(cols_to_check, "[]")
    
    # connect to db table & get colnames
    remote_master_table = dplyr::tbl(db_connection, I(master_table))
    mt_colnames = colnames(remote_master_table)
    
    # missing cols
    missing_cols = setdiff(cols_to_check, mt_colnames)
    
    for(col in missing_cols){
      msg = glue::glue("Column '{col}' not found in the master table.")
      warning(msg)
    }
    
    passes_all_critical_checks = passes_all_critical_checks & length(missing_cols) == 0
  }
  
  ## Measure tables and columns exist in database or in the measure_file ----
  
  distinct_file_and_tables = dplyr::select(control_file, dplyr::all_of(c("measure_file", "measure_table")))
  distinct_file_and_tables = dplyr::distinct(distinct_file_and_tables)
  
  for(row in seq_len(nrow(distinct_file_and_tables))){
    
    ### table ----
    this_file = distinct_file_and_tables$measure_file[row]
    this_table = distinct_file_and_tables$measure_table[row]
    
    measure_table_exists = DBI::dbExistsTable(db_connection, sql2id(this_table))
    file_contents_exist = sql_file_exists_and_contains(file.path(sql_folder, this_file), this_table)
    
    # warnings
    if(!(measure_table_exists | file_contents_exist)){
      msg = glue::glue(
        "Table '{this_table}' not found in database",
        ifelse(is.na(this_file), ".", " or in {this_file}.")
      )
      warning(msg)
      passes_all_critical_checks = FALSE
      next
    }
    
    ### columns ----
    
    # columns to check
    tmp_cf = dplyr::filter(control_file, .data$measure_file == this_file, .data$measure_table == this_table)
    cols_to_check = c(tmp_cf$measure_uid, tmp_cf$measure_start, tmp_cf$measure_end, tmp_cf$measure_value)
    cols_to_check = cols_to_check[is_delimited(cols_to_check, "[]")]
    cols_to_check = unique(cols_to_check)
    cols_to_check = remove_delimiters(cols_to_check, "[]")
    
    if(measure_table_exists){
      # connect to db table & get column names
      remote_measure_table = dplyr::tbl(db_connection, I(this_table))
      measure_colnames = colnames(remote_measure_table)
      
      # missing cols
      missing_cols = setdiff(cols_to_check, measure_colnames)
      
      display_msg = "Column '{col}' not found in table '{this_table}'."
    }
    
    if(file_contents_exist & !measure_table_exists){
      # cols to check in file
      valid_cols = sql_file_exists_and_contains(file.path(sql_folder, this_file), cols_to_check)
      missing_cols = cols_to_check[!valid_cols]
      
      display_msg = "Column '{col}' not found in file '{this_file}'."
    }
    
    for(col in missing_cols){
      msg = glue::glue(display_msg)
      warning(msg)
    }
    passes_all_critical_checks = passes_all_critical_checks & length(missing_cols) == 0
    
  } # end for loop over row
  
  ## Output methods are accepted types ----
  
  accepted_output_methods = c("MIN", "MAX", "SUM", "MEAN", "EXISTS", "COUNT", "DISTINCT", "ENTITY", "DURATION", "SUM_WITHIN")
  actual_output_methods = unique(control_file$output_method)
  invalid_methods = setdiff(tolower(actual_output_methods), tolower(accepted_output_methods))
  
  for(tt in invalid_methods){
    msg = glue::glue("Output method '{tt}' is not an accepted method.")
    warning(msg)
  }
  
  passes_all_critical_checks = passes_all_critical_checks & length(invalid_methods) == 0

  ## Output data types are valid SQL ----
  
  actual_output_types = unique(control_file$output_type)
  invalid_types = actual_output_types[!is_valid_data_type(actual_output_types)]
  
  for(tt in invalid_types){
    msg = glue::glue("Output type '{tt}' is not an accepted data type.")
    warning(msg)
  }
  
  passes_all_critical_checks = passes_all_critical_checks & length(invalid_types) == 0
  
  ## conclude ----
  return(passes_all_critical_checks)
}
