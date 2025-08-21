#' Execute pipeline tool.
#' 
#' @param control_file location of the control file containing pipeline
#' instructions to read into R. Accepted `.csv` and `.xlsx` file formats.
#' @param sheet Sheet to read if control file is `.xlsx` format. As per
#' `openxlsx2::read_xlsx`: either a string (name of a sheet), or an integer
#' (the position of the sheet). Defaults to the first sheet otherwise.
#' @param db_connection_string A connection string for connecting to the
#' database. Only required if SQL files are included in the pipeline.
#' @param delay_minutes Number of minutes to delay execution. Defaults to sixty.
#' Designed for running pipeline out of hours.
#' @param injection_r A list containing named values. For each R script in the
#' pipeline, a separate environment will be created and populated with these
#' variables before the file is executed (see details).
#' @param injection_sql A list containing named values. For each SQL script in
#' the pipeline, where the names are found in the SQL code, these will be
#' replaced with their values (see details).
#' @param ignore_warnings T/F whether execution of R or SQL scripts should
#' continue even if warnings occur. If `FALSE` (the default) then if a warning
#' occurs during the execution of any file in the pipeline, execution of that
#' file will stop. If `TRUE` then will suppress all warnings, files in the
#' pipeline will only stop running if they encounter an error.
#' @param sink_file If provided, a file to sink console progress reports to.
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
#' 
#' As an alternative, the text `STOP IF ANY FAILURES` can be entered can be
#' entered in the `File` column of the control file. This is a special command
#' and will cause the tool to stop if any failures have occurred. This is
#' useful for separating stages of the pipeline where subsequent stages should
#' run only if all of the previous stage(s) complete.
#' 
#' If the `delay_minutes` argument is at least 60, then a random additional
#' delay of 1-60 minutes is added upon execution. This is to prevent all the
#' pipelines executed at 5pm from causing strain on the system when they all
#' attempt to run one hour later at 6pm.
#' 
#' `injection_r` provides a way to insert dynamic values into the environment
#' when R scripts run. For example `injection_r = list(ext = "csv")` would be
#' equivalent to adding the code `ext <- "csv"` at the top of every R script in
#' the pipeline. Case sensitive. `injection_r` exists to allow parameters to be
#' passed to scripts. 
#' 
#' `injection_sql` provides a way to insert dynamic values into an SQL script.
#' It is the equivalent of SQL CMD mode for the pipeline tool (as SQL CMD mode
#' does not work as expected via ODBC connection). Case sensitive. For example
#' `injection_sql = list("$(tbl)" = "[my_table]")` would replace all instances
#' of `$(tbl)` with `[my_table]` in all SQL scripts. `injection_sql` exists to
#' allow parameters to be passed to scripts.
#' 
#' @md
#' 
#' @importFrom  rlang .data
#' @export
run_pipeline = function(
    control_file,
    sheet = NULL,
    db_connection_string = NA_character_,
    delay_minutes = 60,
    injection_r = list(),
    injection_sql = list(),
    ignore_warnings = FALSE,
    sink_file = NULL
){
  stopifnot(is.character(control_file), file.exists(control_file))
  stopifnot(is.null(sheet) | is.character(sheet))
  stopifnot(is.character(db_connection_string))
  stopifnot(is.numeric(delay_minutes))
  stopifnot(ignore_warnings %in% c(TRUE, FALSE))
  stopifnot(is.list(injection_r))
  stopifnot(is.list(injection_sql))
  
  run_time_inform_user("Pipeline tool initiated.")
  
  ## load control file ----
  
  loaded_cf = load_control_file(control_file, sheet = sheet)
  
  # tidy control file column names
  ctr_cols = trimws(tolower(colnames(loaded_cf)))
  colnames(loaded_cf) = ctr_cols
  
  # drop progress reporting columns
  loaded_cf = dplyr::select(loaded_cf, -dplyr::any_of(c("start_time", "end_time", "status")))
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    loaded_cf = dplyr::filter(loaded_cf, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  if(nrow(loaded_cf) == 0){
    warning("All rows of control file disabled, returnig NULL")
    return(NULL)
  }
  
  ## sink for logging ----
  
  if(!is.null(sink_file)){
    sink(sink_file, append = TRUE, split = TRUE)
    on.exit({sink()}, add = TRUE, after = TRUE)
    cat("===============================================================\n")
  }
  
  ## storage for results ----
  
  result_df = dplyr::mutate(
    loaded_cf,
    start_time = NA_character_,
    end_time = NA_character_,
    status = NA_character_
  )

  on.exit(expr = {
    save_control_file_w_progress(control_file, sheet = sheet, result_df)
  }, add = TRUE, after = TRUE)
  
  ## initialize ----
  
  stopifnot("folder" %in% ctr_cols)
  loaded_cf$folder = adjust_file_path_handling(loaded_cf$folder)
  
  valid_control_file = validate_pipeline_control_file(loaded_cf, db_connection_string, injection_sql = injection_sql)
  stopifnot(valid_control_file)
  
  loaded_cf = dplyr::mutate(loaded_cf, full_path = file.path(.data$folder, .data$file))
  
  ## setup for pipeline ----
  
  if("order" %in% ctr_cols){
    loaded_cf$order = suppressWarnings(as.numeric(loaded_cf$order))
    loaded_cf = dplyr::arrange(loaded_cf, .data$order)
  }
  
  ## impose delay if required ----
  
  if(delay_minutes >= 60){
    delay_minutes = delay_minutes + sample(1:60,1)
  }
  
  if(delay_minutes > 0){
    msg = as.character(Sys.time() + 60 * delay_minutes)
    msg = substr(msg, 1, 19)
    msg = paste("Tool waiting until", msg)
    run_time_inform_user(msg)
    
    run_time_inform_user("Entering sleep")
    Sys.sleep(60 * delay_minutes)
    run_time_inform_user("Resuming from sleep")
  }
  
  ## calling handler functions ----
  
  message_handler = function(m) {
    cat("Global message: ", conditionMessage(m))
    invokeRestart("muffleMessage")
  }
  
  warning_handler = function(w) {
    cat("Global message: ", conditionMessage(w), "\n")
    invokeRestart("muffleWarning")
  }
  
  error_handler = function(e) {
    cat("Global error: ", conditionMessage(e), "\n")
    # muffle does not exist for errors
  }
  
  ## run each file ----
  
  withCallingHandlers(
    {
      for(ii in seq_len(nrow(loaded_cf))){
        this_file = loaded_cf$full_path[ii]
        
        if(grepl("STOP IF ANY FAILURES", this_file, fixed = TRUE)){
          run_time_inform_user("Pipeline verifying no failures so far")
          all_success_so_far = all(result_df$status == "Successful completion", na.rm = TRUE)
          
          if(!all_success_so_far){
            run_time_inform_user("Pipeline stop with failures")
            result_df$start_time[ii] = as.character(Sys.time())
            result_df$end_time[ii] = as.character(Sys.time())
            result_df$status[ii] = "Stopped with error: Failures detected"
            break
          }
          
          result_df$start_time[ii] = as.character(Sys.time())
          result_df$end_time[ii] = as.character(Sys.time())
          result_df$status[ii] = "Successful completion"
          next
        }
        
        msg = glue::glue("Pipeline file '{basename(this_file)}': Started")
        run_time_inform_user(msg)
        
        if(tolower(tools::file_ext(this_file)) == "sql"){
          result = try_run_SQL_file(file = this_file, db_connection_string, injection_sql, ignore_warnings)
        }
        if(tolower(tools::file_ext(this_file)) == "r"){
          result = try_run_R_file(file = this_file, injection_r, ignore_warnings)
        }
        
        # conclude
        msg = glue::glue("Pipeline file '{basename(this_file)}': {result$status}.")
        run_time_inform_user(msg)
        
        result_df$start_time[ii] = result$start_time
        result_df$end_time[ii] = result$end_time
        result_df$status[ii] = result$status
      }
    },
    message = message_handler,
    warning = warning_handler,
    error = error_handler
  )
  
  ## conclude ----
  run_time_inform_user("Pipeline tool complete.")
  return(invisible(result_df))
}
