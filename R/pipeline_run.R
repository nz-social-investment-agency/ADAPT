#' Execute pipeline tool.
#' 
#' @param control_file location of the control file containing pipeline
#' instructions to read into R. Accepted `.csv` and `.xlsx` file formats.
#' @param sheet Sheet to read if control file is `.xlsx` format. As per
#' `openxlsx2::read_xlsx`: either a string (name of a sheet), or an integer
#' (the position of the sheet). Defaults to the first sheet otherwise.
#' @param db_connection_string A connection string for connecting to the
#' database. Only required if SQL files are included in the pipeline.
#' @param delay_minutes Number of minutes to delay execution. Defaults to zero.
#' Useful if wanting to run pipeline out of hours.
#' @param ignore_warnings T/F whether execution of R or SQL scripts should
#' continue even if warnings occur. If `FALSE` (the default) then if a warning
#' occurs during the execution of any file in the pipeline, execution of that
#' file will stop. If `TRUE` then will suppress all warnings, files in the
#' pipeline will only stop running if they encounter an error.
#'
#' @return A data frame containing all the files run in order, along with start
#' and end times and the status of their execution (completed, stopped with
#' warning or error). If a warning or error occured, then status also includes
#' the warning or error message.
#'
#' @details
#' The best way to understand the pipeline tool is to review a worked example.
#' Try `provide_example` for worked examples, or for example control files.
#' 
#' For each row in the control file, the pipeline tool executes the file using
#' either `try_run_R_file` or `try_run_SQL_file`, depending on the file
#' extension.
#' 
#' Control files are validated prior to execution. To validate a control file
#' without execution use `validate_pipeline_control_file`.
#' 
#' The accepted columns for the control file are:
#' * Enabled - Optional but recommended column, containing True / False or
#' yes / no. Allows for rows of the control file to be turned on and off.
#' * Order - Optional column for setting the order of the files to be run.
#' Files without an order are run in the order they appear in the control file
#' after all files with an order.
#' * Folder - The folder that contains the file to be executed. All folders are
#' processed with `adjust_file_path_handling` which converts format to match
#' R and data lab expectations.
#' * File - The file to be executed. Only R and SQL scripts are accepted.
#' * Notes - free text column for adding notes to the control file. This column
#' is ignored during execution and does not effect output. Any other column
#' names are also ignored, but generate a warning.
#' @md
#' 
#' @importFrom  rlang .data
#' @export
run_pipeline = function(control_file, sheet = NULL, db_connection_string = NA_character_, delay_minutes = 0, ignore_warnings = FALSE){
  stopifnot(is.character(control_file), file.exists(control_file))
  stopifnot(is.null(sheet) | is.character(sheet))
  stopifnot(is.character(db_connection_string))
  stopifnot(is.numeric(delay_minutes))
  stopifnot(ignore_warnings %in% c(TRUE, FALSE))
  
  run_time_inform_user("Pipeline tool initiated.")
  
  ## load control file ----
  
  loaded_cf = load_control_file(control_file, sheet = sheet)
  # drop progress reporting columns
  loaded_cf = dplyr::select(loaded_cf, -dplyr::any_of(c("start_time", "end_time", "status")))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(loaded_cf)))
  colnames(loaded_cf) = ctr_cols
  
  valid_control_file = validate_pipeline_control_file(loaded_cf, db_connection_string)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    loaded_cf = dplyr::filter(loaded_cf, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  if(nrow(loaded_cf) == 0){
    warning("All rows of control file disabled, returnig NULL")
    return(NULL)
  }
  
  loaded_cf$folder = sapply(loaded_cf$folder, adjust_file_path_handling, USE.NAMES = FALSE)
  loaded_cf = dplyr::mutate(loaded_cf, full_path = file.path(.data$folder, .data$file))
  
  ## setup for pipeline ----
  
  if("order" %in% ctr_cols){
    loaded_cf = dplyr::arrange(loaded_cf, .data$order)
  }
  
  any_sql = any(tolower(tools::file_ext(loaded_cf$file)) == "sql")
  if(any_sql){
    db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)
    on.exit(DBI::dbDisconnect(db_connection))
  }
  
  ## impose delay if required ----
  
  if(delay_minutes > 0){
    msg = as.character(Sys.time() + 60 * delay_minutes)
    msg = substr(msg, 1, 19)
    msg = paste("Tool waiting until", msg)
    run_time_inform_user(msg)
    
    run_time_inform_user("Entering sleep")
    Sys.sleep(60 * delay_minutes)
    run_time_inform_user("Resuming from sleep")
  }
  
  ## run each file ----
  
  result_list = list()
  
  for(ff in loaded_cf$full_path){
    msg = glue::glue("Pipeline file '{basename(ff)}': Started")
    run_time_inform_user(msg)
    
    if(tolower(tools::file_ext(ff)) == "sql"){
      result = try_run_SQL_file(file = ff, db_connection_string, ignore_warnings)
    }
    if(tolower(tools::file_ext(ff)) == "r"){
      result = try_run_R_file(file = ff, ignore_warnings)
    }
    result = c(full_path = ff, result)
    msg = glue::glue("Pipeline file '{basename(ff)}': {result$status}.")
    run_time_inform_user(msg)
    result_list = c(result_list, list(result))
  }
  
  ## results df ----
  
  result_df = dplyr::bind_rows(result_list)
  result_df = dplyr::mutate(
    result_df,
    file = basename(.data$full_path),
    folder = dirname(.data$full_path)
  )
  result_df = dplyr::select(result_df, dplyr::all_of(c("folder", "file", "status", "start_time", "end_time")))
  
  ## conclude ----
  save_control_file_w_progress(control_file, sheet = sheet, result_df)
  run_time_inform_user("Pipeline tool complete.")
  return(invisible(result_df))
}
