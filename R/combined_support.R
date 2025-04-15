## Provide example files -------------------------------------------------- ----
#' Provide an examples of the tools - copying example files from the package
#' data into a local folder. Includes example control files and worked examples
#' of the tool in use.
#' 
#' @param example The example to copy. If does not match an available example,
#' then prints out a list of the available examples.
#' @param folder Folder to copy the example into. Creates the folder if it does
#' not already exist.
#'
#' @return The path of any newly created file(s).
#' 
#' @examplesIf interactive()
#' # list all the available examples
#' provide_example()
#' 
#' # load a specific example
#' provide_example("summary_control_file", "./example")
#' 
#' @export
provide_example = function(example = NA_character_, folder = "."){
  stopifnot(is.character(folder))
  stopifnot(is.character(example))
  
  example_folder = system.file("extdata", "examples", package = "IDIr")
  available_examples = list.dirs(example_folder, full.names = FALSE, recursive = FALSE)
  
  # list available examples is one is not selected
  if(!example %in% available_examples){
    msg = paste0(available_examples, collapse = '\n')
    msg = glue::glue("Available examples:\n{msg}")
    message(msg)
    return(invisible(available_examples))
  }
  
  # locations
  from_dir = file.path(example_folder, example)
  to_dir = file.path(folder)
  
  if(!dir.exists(to_dir)){
    dir.create(to_dir)
  }
  
  # copy
  from_files = list.files(from_dir)
  file.copy(file.path(from_dir, from_files), to_dir)
  
  # conclude
  new_files = file.path(to_dir, from_files)
  return(new_files)
}

## Adjust file paths ------------------------------------------------------ ----
#' Adjust Windows paths to R paths
#' 
#' @param file_name_and_path The file path and folder to adjust.
#' 
#' @return The input adjusted for R pathing.
#' 
#' @details
#' Converts forward slashes to back slashes and replaces drive letter with
#' Data Lab location prefix. The Data Lab location prefix is specific to the
#' IDI at time of development.
#' 
#' The inclusion of this function allows researchers to copy-and-paste paths
#' from Windows into control files.
#' 
#' @export
adjust_file_path_handling = function(file_name_and_path){
  stopifnot(is.character(file_name_and_path))
  
  # adjust if needed
  file_name_and_path = gsub("\\\\", "/", file_name_and_path)
  file_name_and_path = gsub("//", "/", file_name_and_path)
  
  file_name_and_path = gsub("^[a-zA-Z]:/MAA/", "/nas/DataLab/MAA/", file_name_and_path)
  file_name_and_path = gsub("^[a-zA-Z]:/IMR/", "/nas/DataLab/IMR/", file_name_and_path)
  
  file_name_and_path = gsub("^[a-zA-Z]:/MAA20", "/nas/DataLab/MAA/MAA20", file_name_and_path)
  file_name_and_path = gsub("^[a-zA-Z]:/IMR20", "/nas/DataLab/IMR/MAA20", file_name_and_path)
  
  # warn if paths may be out of date
  in_data_lab = within_data_lab()
  MAA_out_of_date = in_data_lab & grepl("MAA[0-9]", file_name_and_path) & !dir.exists("/nas/DataLab/MAA/")
  IMR_out_of_date = in_data_lab & grepl("IMR[0-9]", file_name_and_path) & !dir.exists("/nas/DataLab/IMR/")
  if(any(MAA_out_of_date) | any(IMR_out_of_date)){
    warning("data lab paths in IDIr may be out of date")
  }
  
  return(file_name_and_path)
}

## Load control file ------------------------------------------------------ ----
#' Auto-detecting which file read command is needed
#'
#' Used to support csv and xlsx formatted control files.
#'
#' @param path_and_file_name location of the file to read into R
#' @param sheet Sheet to read if Excel file as per `openxlsx2::read_xlsx`:
#' Either a string (name of a sheet), or an integer (the position of the
#' sheet). Defaults to the first sheet otherwise.
#'
#' @return the file contents as a data.frame
#'
#' @examplesIf interactive()
#' example_control_file = example_summary_control_file(".")
#' control_file = load_control_file(example_control_file[1])
#' 
#' @export
load_control_file = function(path_and_file_name, sheet = NULL){
  stopifnot(is.character(path_and_file_name))
  stopifnot(file.exists(path_and_file_name))
  stopifnot(is.null(sheet) | is.character(sheet))
  extension = tolower(tools::file_ext(path_and_file_name))
  stopifnot(extension %in% c("xlsx", "csv"))
  
  # load file
  if (extension == "xlsx") {
    file_contents = openxlsx2::read_xlsx(path_and_file_name, sheet = sheet, convert = FALSE)
  }
  if (extension == "csv") {
    file_contents = utils::read.csv(path_and_file_name, stringsAsFactors = FALSE, colClasses = "character")
  }

  # trim all white space
  file_contents = as.data.frame(file_contents, stringsAsFactors = FALSE)
  file_contents = data.frame(lapply(file_contents, trimws))
  
  # 0 characters to NA
  file_contents = data.frame(lapply(file_contents, function(x){ifelse(is.na(x) | nchar(x) == 0, NA_character_, x)}))
  
  # drop rows that are all NA
  file_contents = file_contents[!apply(is.na(file_contents), 1, all), ]
  
  # order to numeric if exists
  order_col = grepl("^order$", colnames(file_contents), ignore.case = TRUE)
  if(any(order_col)){
    order_col = colnames(file_contents)[order_col]
    file_contents[[order_col]] = as.numeric(file_contents[[order_col]])
  }
  
  return(file_contents)
}

## Save control file with progress reporting ------------------------------ ----
#' Save progress update to control file
#'
#' @param path_and_file_name location of the file to read into R
#' @param sheet Sheet to read if Excel file as per `openxlsx2::read_xlsx`:
#' Either a string (name of a sheet), or an integer (the position of the
#' sheet). Defaults to the first sheet otherwise.
#' @param progress_df A data frame with the columns start_time, end_time, and
#' status. And all other columns in common with the control file.
#' @param overwrite T/F should the existing file be overwritten. If FALSE, will
#' increment the file name (for example: `file.csv` becomes `file (1).csv`).
#' If TRUE will attempt to overwrite and if overwrite fails, will increment the
#' file name.
#'
#' @return The path of the written file.
#' 
save_control_file_w_progress = function(path_and_file_name, sheet = NULL, progress_df, overwrite = TRUE){
  stopifnot(is.character(path_and_file_name))
  stopifnot(file.exists(path_and_file_name))
  stopifnot(is.null(sheet) | is.character(sheet))
  extension = tolower(tools::file_ext(path_and_file_name))
  stopifnot(extension %in% c("xlsx", "csv"))
  stopifnot(is.data.frame(progress_df))
  stopifnot(overwrite %in% c(TRUE, FALSE))
  
  ## control file prep ----
  
  # load control file
  control_file = load_control_file(path_and_file_name, sheet = sheet)
  control_file = merge_progress_into_control_file(control_file, progress_df)
  
  ## write xlsx ----
  if (extension == "xlsx") {
    
    wb = openxlsx2::wb_load(path_and_file_name)
    wb = openxlsx2::wb_clean_sheet(wb, sheet)
    wb = openxlsx2::wb_add_data(wb, sheet, x = control_file, na.strings = "")
    
    if(!overwrite){
      path_and_file_name = increment_file_name(path_and_file_name)
    }
    
    path_and_file_name = tryCatch({
      msg = glue::glue("Writing progress to '{basename(path_and_file_name)}'.")
      run_time_inform_user(msg)
      openxlsx2::wb_save(wb, path_and_file_name)
      path_and_file_name
    },
    error = function(e){
      msg = glue::glue("Writing progress to '{basename(path_and_file_name)}' failed.")
      run_time_inform_user(msg)
      path_and_file_name = increment_file_name(path_and_file_name)
      msg = glue::glue("Writing progress to '{basename(path_and_file_name)}' instead.")
      run_time_inform_user(msg)
      openxlsx2::wb_save(wb, path_and_file_name)
      return(path_and_file_name)
    })
  }
  
  ## write csv ----
  if (extension == "csv") {
    
    if(!overwrite){
      path_and_file_name = increment_file_name(path_and_file_name)
    }
    
    path_and_file_name = tryCatch({
      msg = glue::glue("Writing progress to '{basename(path_and_file_name)}'.")
      run_time_inform_user(msg)
      utils::write.csv(control_file, path_and_file_name, row.names = FALSE, na = "")
      path_and_file_name
    },
    error = function(e){
      msg = glue::glue("Writing progress to '{basename(path_and_file_name)}' failed.")
      run_time_inform_user(msg)
      path_and_file_name = increment_file_name(path_and_file_name)
      msg = glue::glue("Writing progress to '{basename(path_and_file_name)}' instead.")
      run_time_inform_user(msg)
      utils::write.csv(control_file, path_and_file_name, row.names = FALSE)
      return(path_and_file_name)
    })
  }
  
  ## conclude ----
  return(path_and_file_name)
}

## Merge control file and progress report --------------------------------- ----
#' Merge progress information into control file
#'
#' @param control_file A loaded control file to update.
#' @param progress_df A data frame with the columns start_time, end_time, and
#' status. And all other columns in common with the control file.
#' 
#' @return The control file with progress information merged in.
#' 
#' @details
#' Merging only occurs when rows are enabled. Rows that have been disabled are
#' not updated.
#' 
merge_progress_into_control_file = function(control_file, progress_df){
  stopifnot(is.data.frame(control_file), is.data.frame(progress_df))
  req_cols = c("start_time", "end_time", "status")
  stopifnot(all(req_cols %in% colnames(progress_df)))
  
  # add reporting columns if missing
  for(cc in req_cols){
    if(cc %in% colnames(control_file)){ next }
    control_file[[cc]] = NA_character_
  }
  
  # case handling
  current_colnames = colnames(control_file)
  colnames(control_file) = tolower(colnames(control_file))
  
  # join
  control_file = dplyr::left_join(
    control_file,
    progress_df,
    by = setdiff(colnames(progress_df), c("start_time", "end_time", "status")),
    suffix = c("_cf", "_p")
  )
  
  # column with 'enabled'
  enabled_column = colnames(control_file)[tolower(colnames(control_file)) == "enabled"]
  if(length(enabled_column) == 0){
    control_file$temp_writing_enabled = TRUE
  } else {
    control_file$temp_writing_enabled = tolower(control_file[[enabled_column]]) %in% c("true", "1", "t", "yes", "y")
  }
  
  # merge reporting columns
  control_file = dplyr::mutate(
    control_file,
    start_time = ifelse(.data$temp_writing_enabled, .data$start_time_p, .data$start_time_cf),
    end_time = ifelse(.data$temp_writing_enabled, .data$end_time_p, .data$end_time_cf),
    status = ifelse(.data$temp_writing_enabled, .data$status_p, .data$status_cf)
  )
  
  # remove merging columns
  control_file = dplyr::select(control_file, dplyr::any_of(tolower(current_colnames)))
  colnames(control_file) = current_colnames
  
  return(control_file)
}

## Save code to file ------------------------------------------------------ ----
#' Save code to file for debugging
#' 
#' For transparency and ease of debugging, this function simplifies writing to
#' disc key code components. It provides a standardized way to save SQL code
#' or character strings using within dynamic dplyr code.
#' 
#' @param query the text of the query to save. This may be generated using
#' `dplyr::show_query` where required.
#' @param desc a description of the query. Use to name the file. If an
#' extension is included this extension is used. Otherwise .txt is used.
#' @param folder_path The path to save the query. If provided will attempt to
#' save a copy of the query to the provided folder. Errors if directory does
#' not exist.
#' 
#' @return The location and name of the saved file.
#' 
#' @examplesIf interactive()
#' code = "SELECT * FROM mytable"
#' written_file = save_code_to_script(code, "saved code", ".")
#' 
#' @export
save_code_to_script = function(query, desc, folder_path) {
  stopifnot(dbplyr::is.sql(query) | is.character(query))
  stopifnot(is.character(desc))
  stopifnot(is.character(folder_path))
  stopifnot(dir.exists(folder_path))
  
  # tiny delay ensures no two files writes can have the same time-stamp
  Sys.sleep(0.1)
  
  # add extension if missing
  extension = tools::file_ext(desc)
  extension = ifelse(extension == "", "txt", extension)
  # remove extension from file description
  desc = gsub(glue::glue("\\.{extension}"), "", desc)
  
  # time stamp includes milliseconds
  clean_time = gsub("[.:]", "-", format(Sys.time(), "%Y-%m-%d %H%M%OS3"))
  clean_name = gsub("[. :]", "_", desc)
  file_name = glue::glue("{clean_time} {clean_name}.{extension}")
  
  tryCatch(
    # try to write file
    {
      # create directory if required
      writeLines(as.character(query), file.path(folder_path, file_name))
      return(file.path(folder_path, file_name))
    },
    # if error > display as warning
    error = function(e){
      msg = glue::glue("Error while saving query text:\n{e}")
      warning(msg)
    },
    # if warning > display
    warning = function(w){
      msg = glue::glue("Waring while saving query text:\n{w}")
      warning(msg)
    }
  )
  
  return(invisible())
}

## Colnames to lower ------------------------------------------------------ ----
#' Convert column names to lower case
#' 
#' R is case sensitive, but most SQL databases are case insensitive, and user
#' input in control files is not guaranteed to be case sensitive. One simple
#' solution is to convert all column names to lower case. This function does so
#' in a way that is dbplyr compatible for remote tables.
#' 
#' @param tbl a data frame to rename. Can be in-memory or remote accessed
#' with dbplyr.
#' 
tolower_colnames = function(tbl){
  stopifnot(is.data.frame(tbl) | dplyr::is.tbl(tbl))
  
  current_colnames = colnames(tbl)
  new_colnames = tolower(current_colnames)
  
  if(length(unique(new_colnames)) != length(current_colnames)){
    stop("Column names must be unique without capitalisation")
  }
  
  rename_command = current_colnames
  names(rename_command) = new_colnames
  
  tbl = dplyr::rename(tbl, !!!rlang::parse_exprs(rename_command))
  return(tbl)
}
