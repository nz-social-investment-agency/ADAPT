################################################################################
#' Notes
#' 
################################################################################

## Provide files ---------------------------------------------------------- ----
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

## Load control file ------------------------------------------------------ ----
#' Auto-detecting which file read command is needed
#'
#' Used to support csv, xls, and xlsx formatted control files.
#'
#' @param path_and_file_name location of the file to read into R
#' @param sheet Sheet to read if Excel file as per `readxl::read_excel`:
#' Either a string (name of a sheet), #' or an integer (the position of the
#' sheet). Defaults to the first sheet otherwise.
#'
#' @return the file contents as a data.frame
#'
#' @examplesIf interactive()
#' example_control_file = example_summary_control_file(".")
#' control_file = load_control_file(example_control_file[1])
#' 
load_control_file = function(path_and_file_name, sheet = NULL){
  stopifnot(is.character(path_and_file_name))
  stopifnot(file.exists(path_and_file_name))
  
  extension = tolower(tools::file_ext(path_and_file_name))
  stopifnot(extension %in% c("xlsx", "xls", "csv"))
  
  # load file
  if (extension == "xls") {
    file_contents = readxl::read_xls(path_and_file_name, sheet = sheet, col_types = "text")
  }
  if (extension == "xlsx") {
    file_contents = readxl::read_xlsx(path_and_file_name, sheet = sheet, col_types = "text")
  }
  if (extension == "csv") {
    file_contents = utils::read.csv(path_and_file_name, stringsAsFactors = FALSE, colClasses = "character")
  }

  # trim all white space
  file_contents = as.data.frame(file_contents, stringsAsFactors = FALSE)
  file_contents = data.frame(lapply(file_contents, trimws))
  
  # 0 characters to NA
  file_contents = data.frame(lapply(file_contents, function(x){ifelse(nchar(x) == 0, NA_character_, x)}))
  
  # drop rows that are all NA
  file_contents = file_contents[!apply(is.na(file_contents), 1, all), ]
  
  return(file_contents)
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
