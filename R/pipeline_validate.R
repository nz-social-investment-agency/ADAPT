#' Confirm that pipeline file instructions can be executed.
#' 
#' @param control_file a data frame containing assembly instructions. Most
#' likely read into memory by `load_control_file`.
#' @param db_connection_string A connection string for connecting to the
#' database. Only required if SQL files are included in the pipeline.
#'
#' @return T/F whether or not all validation checks are passed. Generating
#' warnings for all failed checks.
#'
#' @details
#' The following checks are run and generate a failure if not passed:
#' * Required column names are present.
#' * File names are accepted extension.
#' * If any `.sql` files, then confirm database connection works
#' * All folders exist
#' * All files exist in their folders
#' * SQL files generate no errors on parse (this does not guarantee
#' files can be executed without error as scripts have not been compiled or
#' executed, but it helps catch the most obvious things).
#' 
#' The following checks are run and only generate a warning if not passed:
#' * Unrecognised column names are present.
#' @md
#' 
#' @importFrom  rlang .data
#' @export
validate_pipeline_control_file = function(control_file, db_connection_string = NA_character_){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.character(db_connection_string))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  # sort if applicable
  if("order" %in% ctr_cols){
    control_file = dplyr::arrange(control_file, .data$order)
  }
  
  ## required columns are present ----
  
  expected_columns_names = c("folder", "file")
  missing_cols = setdiff(expected_columns_names, colnames(control_file))
  
  for(col in missing_cols){
    msg = glue::glue("Control file is missing required column '{col}'.")
    warning(msg)
  }
  
  if(length(missing_cols) > 0){
    return(FALSE)
  }
  
  ## Unrecognised column names are present (no error) ----
  
  optional_cols = c("enabled", "order", "folder", "file", "note", "notes", "start_time", "end_time", "status")
  extra_cols = setdiff(colnames(control_file), optional_cols)
  
  for(col in extra_cols){
    msg = glue::glue("Unrecognised control file column '{col}' ignored.")
    warning(msg)
  }
  
  ## setup for checks ----
  
  control_file$folder = sapply(control_file$folder, adjust_file_path_handling, USE.NAMES = FALSE)
  control_file = dplyr::mutate(control_file, full_path = file.path(.data$folder, .data$file))
  
  # remove STOP IF ANY FAILURES
  control_file = dplyr::filter(control_file, file != "STOP IF ANY FAILURES")
  
  # track passing of checks
  passes_all_critical_checks = TRUE
  
  ## accepted file extensions ----
  
  extensions = tools::file_ext(control_file$file)
  unaccepted = tolower(extensions) %not_in% c("r", "sql")
  
  for(ff in control_file$file[unaccepted]){
    msg = glue::glue("File '{ff}' has unaccepted extension: '{tools::file_ext(ff)}' is neither R nor SQL.")
    warning(msg)
  }
  
  passes_all_critical_checks = passes_all_critical_checks & all(!unaccepted)
  control_file = dplyr::filter(control_file, !unaccepted)
  
  ## valid db_connection ----
  
  any_sql = any(tolower(extensions) == "sql")
  can_connect = FALSE
  
  if(any_sql){
    can_connect = DBI::dbCanConnect(odbc::odbc(), .connection_string = db_connection_string)
    
    if(!can_connect){
      warning("Unable to connect to database with connection string")
      passes_all_critical_checks = FALSE
    }
  }
  
  ## all folders exist ----
  
  folders = unique(control_file$folder)
  unaccepted = !dir.exists(folders)
  
  for(ff in folders[unaccepted]){
    msg = glue::glue("Folder '{ff}' does not exist.")
    warning(msg)
  }
  passes_all_critical_checks = passes_all_critical_checks & all(!unaccepted)
  control_file = dplyr::filter(control_file, .data$folder %not_in% folders[unaccepted])
  
  ## all files exist ----
  
  files = unique(control_file$full_path)
  unaccepted = !file.exists(files)
  
  for(ff in files[unaccepted]){
    msg = glue::glue("File '{ff}' does not exist.")
    warning(msg)
  }
  passes_all_critical_checks = passes_all_critical_checks & all(!unaccepted)
  control_file = dplyr::filter(control_file, .data$full_path %not_in% files[unaccepted])
  
  ## sql files pass NOEXEC check ----
  
  if(any_sql & can_connect){
    # subset to sql files
    is_sql = tolower(tools::file_ext(control_file$file)) == "sql"
    
    # connect and set NOEXEC on
    db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)
    on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
    
    DBI::dbExecute(db_connection, "SET PARSEONLY ON")
    
    # read and split files into batches
    all_batches = lapply(
      control_file$full_path[is_sql],
      function(x){ c(read_and_prepare_sql_code(x), list(file = x)) }
    )
    # convert to data frame
    all_batches = dplyr::bind_rows(all_batches)
    
    # try dbExecute each batch
    batch_error = FALSE
    for(ii in seq_len(nrow(all_batches))){
      
      tryCatch({
        tmp = batch_error
        batch_error = TRUE
        DBI::dbExecute(db_connection, all_batches$code[ii])
        batch_error = tmp
      },
      error = function(e){
        msg = glue::glue(
          "Error in file '{basename(all_batches$file[ii])}'.\n",
          "Between lines {all_batches$start_lines[ii]} to {all_batches$end_lines[ii]}.\n",
          "Message: {e$message}"
        )
        warning(msg)
      })
      
    }
    
    passes_all_critical_checks = passes_all_critical_checks & !batch_error    
  } # end if
  
  ## conclude ----
  return(passes_all_critical_checks)
}
