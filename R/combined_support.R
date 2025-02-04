################################################################################
#' Notes
#' - load_control_file needs a more general solution to column names to handle
#' different control file layouts
#' - load_control_file needs handling for XLSX sheets
#' 
################################################################################

## Provide an example of control file ------------------------------------- ----
#' Provide an example of the control file for a tool
#'
#' @param folder Folder to copy the example into.
#' @param tool Tool to fetch control file example for. Matches internal package
#' extdata folder. Options: assembly, summary, confidential, combined.
#'
#' @return The path of the newly created file(s)
#'
#' @examplesIf interactive()
#' example_control_file("./examples", "summary")
#'
example_control_file = function(folder, tool){
  stopifnot(is.character(folder))
  stopifnot(is.character(tool))
  stopifnot(tool %in% c("assembly", "summary", "confidential", "combined"))
  
  # to location
  to_dir = file.path(normalizePath(folder))
  if(!dir.exists(to_dir)){
    dir.create(to_dir)
  }
  
  # from location
  example_folder = system.file("extdata", "example_control_files", tool, package = "IDIr")
  from_files = list.files(example_folder)
  
  # copy
  file.copy(file.path(example_folder, from_files), to_dir)
  
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
#'
#' @return the file contents as a data.frame
#'
load_control_file = function(path_and_file_name){
  stopifnot(is.character(path_and_file_name))
  stopifnot(file.exists(path_and_file_name))
  
  extension = tolower(tools::file_ext(path_and_file_name))
  stopifnot(extension %in% c("xlsx", "xls", "csv"))
  
  # load file
  if (extension == "xls") {
    file_contents = readxl::read_xls(path_and_file_name, col_types = "character")
  }
  if (extension == "xlsx") {
    file_contents = readxl::read_xlsx(path_and_file_name, col_types = "character")
  }
  if (extension == "csv") {
    file_contents = utils::read.csv(path_and_file_name, stringsAsFactors = FALSE, colClasses = "character")
  }
  
  # standardize column names ### requires a more general solution to work across all control files
  cols = colnames(file_contents)
  cols = gsub("[0-9\\.]", "", cols)
  cols = sapply(1:length(cols), function(ii){paste0(cols[ii], sum(cols[ii] == cols[1:ii]))})
  colnames(file_contents) = cols
  
  # trim all white space
  file_contents = as.data.frame(file_contents, stringsAsFactors = FALSE)
  file_contents = data.frame(lapply(file_contents, trimws))
  
  # 0 characters to NA
  file_contents = data.frame(lapply(file_contents, function(x){ifelse(nchar(x) == 0, NA_character_, x)}))
  
  # drop rows that are all NA
  file_contents = file_contents[!apply(is.na(file_contents), 1, all), ]  
  
  return(file_contents)
}
