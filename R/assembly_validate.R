################################################################################
#' Notes
#' 
################################################################################

#' Confirm that control file instructions can be executed.
#'
#' @param control_file a data frame containing assembly instructions. Most
#' likely read into memory by `load_control_file`.
#' @param connection_string A connection string for the database containing
#' the data, where assembly should take place.
#' @param master_table The name of the table onto which columns should be
#' assembled. It is recommended using the full table name: database.schema.table
#' @param sql_folder The folder location containing SQL scripts. Will only be
#' used if tables listed in the control file can not be found in the database
#' and might have to be generated from SQL scripts.
#'
#' @return T/F whether or not all validation checks are passed. Generating
#' warnings for all failed checks.
#'
#' @details
#' The following checks are run and generate a failure if not passed:
#' * Required column names are present.
#' * Inputs match expected formats / delimiters.
#' * Master table exists.
#' * Master table columns exist.
#' * Measure tables exist in the database or are found in the measure_file.
#' * Measure columns exist in the database or are found in the measure_file.
#' * Output methods are accepted types.
#' 
#' The following checks are run and only generate a warning if not passed:
#' * Unrecognised column names are present.
#' 
#' @md
#' 
#' @importFrom  rlang .data
#' @export
validate_assembly_control_file = function(control_file, connection_string, master_table, sql_folder = "."){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.character(connection_string))
  stopifnot(is.character(master_table))
  stopifnot(is.character(sql_folder))
  stopifnot(dir.exists(sql_folder))
  
  ## connect to database, ensuring connection string works ----
  
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
  stopifnot(DBI::dbIsValid(db_conn))
  on.exit(DBI::dbDisconnect(db_conn))
  
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
  
  req_cols = c("populaiton_uid", "period_start", "period_end", 
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
        accepted = rownames(column_reqs)[column_reqs[[col]]]
        msg = glue::glue("Unaccepted input in column '{col}', row '{row}'\n",
                         "Accepted values are {accepted}")
        warning(msg)
      }
    } # end row iteration
  } # end column iteration
  
  ## Master table exists ----
  
  master_table_exists = DBI::dbExistsTable(db_conn, dplyr::sql(master_table))
  
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
    
    # connect to db table & get colnames
    remote_master_table = dplyr::tbl(db_conn, I(master_table))
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
  
  for(row in seq_along(nrow(distinct_file_and_tables))){
    
    ### table ----
    this_file = distinct_file_and_tables$measure_file[row]
    this_table = distinct_file_and_tables$measure_table[row]
    
    measure_table_exists = DBI::dbExistsTable(db_conn, dplyr::sql(this_table))
    file_contents_exist = sql_file_exists_and_contains(this_file, this_table)
    
    # warnings
    if(!(measure_table_exists | file_contents_exist)){
      msg = glue::glue("Table '{this_table}' not found in database or in '{this_file}'.")
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
    
    if(measure_table_exists){
      # connect to db table & get colnames
      remote_measure_table = dplyr::tbl(db_conn, I(this_table))
      measure_colnames = colnames(remote_measure_table)
      
      # missing cols
      missing_cols = setdiff(cols_to_check, measure_colnames)
      
      msg = "Column '{col}' not found in table '{this_table}'."
    }
    
    if(file_contents_exist & !measure_table_exists){
      # cols to check in file
      valid_cols = sql_file_exists_and_contains(this_file, cols_to_check)
      missing_cols = cols_to_check[!valid_cols]
      
      msg = "Column '{col}' not found in file '{this_file}'."
    }
    
    for(col in missing_cols){
      msg = glue::glue(msg)
      warning(msg)
    }
    passes_all_critical_checks = passes_all_critical_checks & length(missing_cols) == 0
    
  }
  
  ## Output methods are accepted types ----
  
  accepted_output_methods = c("MIN", "MAX", "SUM", "COUNT", "DISTINCT", "ENTITY", "DURATION", "SUM_WITHIN")
  actual_output_methods = unique(control_file$output_method)
  invalid_methods = setdiff(tolower(actual_output_methods), tolower(accepted_output_methods))
  
  for(tt in invalid_methods){
    msg = glue::glue("Output method '{tt}' is not an accepted method.")
    warning(msg)
  }
  
  passes_all_critical_checks = passes_all_critical_checks & length(invalid_methods) == 0

  ## conclude ----
  return(passes_all_critical_checks)
}
