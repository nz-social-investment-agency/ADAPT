## Not In ----------------------------------------------------------------- ----
#' Negative of %in%
#'
#' @param x an array of elements
#' @param y an array of elements
#' 
#' @return T/F for each element in `x` whether the element does NOT appear in
#' `y`. Conversion may occur implicitly between ata types.
#' 
"%not_in%" = function(x, y) {
  !("%in%"(x, y))
}

## Time-stamped info messages --------------------------------------------- ----
#' Prints to console time of function call followed by msg.
#' 
#' @param msg A message to display time stamped for the user.
#' @param log Optional path to an existing log to write to.
#' 
#' @return The time-stamped message invisibly.
#' 
#' @export
run_time_inform_user = function(msg, log = NA_character_) {
  stopifnot(is.character(msg))
  stopifnot(is.character(log))
  stopifnot(is.na(log) | file.exists(log))
  
  now = as.character(Sys.time())
  now = substr(now, 1, 19)
  msg = paste0(now, " | ", msg)
  cat(msg, "\n")
  
  if(!is.na(log)){
    write(msg, log, append = TRUE)
  }
  
  return(invisible(msg))
}

## Add delimiters --------------------------------------------------------- ----
#' Add delimiters to text string
#' 
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return the string with the specified delimiters added (if necessary).
#' 
add_delimiters = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) >= 1)
  stopifnot(nchar(delimiter) <= 2)
  
  n_str = nchar(string)
  n_delim = nchar(delimiter)
  
  first_char_match = substr(string, 1, 1) == substr(delimiter, 1, 1)
  first_char = ifelse(first_char_match, "", substr(delimiter, 1, 1))
  last_char_match = substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)
  last_char = ifelse(last_char_match, "", substr(delimiter, n_delim, n_delim))
  
  return(paste0(first_char, string, last_char))
}

## Remove delimiters ------------------------------------------------------ ----
#' Remove delimiters from text string
#' 
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return the string with the specified delimiters removed (if necessary).
#' 
remove_delimiters = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) >= 1)
  stopifnot(nchar(delimiter) <= 2)
  
  n_str = nchar(string)
  n_delim = nchar(delimiter)
  
  first_char_match = substr(string, 1, 1) == substr(delimiter, 1, 1)
  first_char = ifelse(first_char_match, 2, 1)
  last_char_match = substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)
  last_char = ifelse(last_char_match, n_str - 1, n_str)
  
  return(trimws(substr(string, first_char, last_char)))
}

## Check if delimited ----------------------------------------------------- ----
#' Check string for delimiter
#' 
#' The entries in the input control tables should be delimited as either
#' [] for sql columns or "" for strings
#' This lets us run a check for the right delimiter, e.g.
#' is_delimited(string, "[]")
#' is_delimited(string, "\"")
#' 
#' @param string the string to check for delimiter
#' @param delimiter a 1 or 2 character string containing the delimiter
#' 
#' @return T/F if the string is delimited
#' 
is_delimited = function(string, delimiter) {
  stopifnot(is.character(string))
  stopifnot(is.character(delimiter))
  stopifnot(nchar(delimiter) %in% 1:2)
  
  n_str = nchar(string)
  n_delim = nchar(delimiter)
  
  string_longer_than_delimiters = n_str >= n_delim
  first_char_delimited = substr(string, 1, 1) == substr(delimiter, 1, 1)
  last_char_delimited = substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)
  
  return(string_longer_than_delimiters & first_char_delimited & last_char_delimited)
}

## No obvious code injection risk ----------------------------------------- ----
#' Check input string for obvious markers of code injection.
#' 
#' Identifies special characters `;{}`, unmatched quotes, and unmatched
#' brackets. Helps prevent escaping code injection, but not a complete solution.
#' 
#' @param string a string to be checked for special characters.
#' 
#' @return TRUE if no special characters in string and any quotes are matched,
#' FALSE if special characters or unmatched quotes found.
#' 
no_obvious_escaping_injection = function(string) {
  stopifnot(is.character(string))
  stopifnot(length(string) <= 1)
  
  string = strsplit(string, "")[[1]]
  
  SPECIAL_CHARACTERS = c(";", "{", "}")
  no_special_characters = all(SPECIAL_CHARACTERS %not_in% string)
  
  unmatched_single_quote = sum(string == "'") %% 2 == 0
  unmatched_double_quote = sum(string == "\"") %% 2 == 0
  unmatched_baktik_quote = sum(string == "`") %% 2 == 0
  no_unmatched_quotes = unmatched_single_quote & unmatched_double_quote & unmatched_baktik_quote
  
  unmatched_round_bracket = sum(string == "(" | string == ")") %% 2 == 0
  unmatched_square_bracket = sum(string == "[" | string == "]") %% 2 == 0
  no_unmatched_brackets = unmatched_round_bracket & unmatched_square_bracket
  
  return(no_special_characters & no_unmatched_quotes & no_unmatched_brackets)
}

## SQL character string to Id --------------------------------------------- ----
#' Convert character string of SQL name to Id
#' 
#' @param sql_string A string containing the name of an SQL object, most likely
#' delimited with square brackets.
#' 
#' @returns A `DBI::Id` object converting from splitting `sql_string` by `.`
#' and removing `[]` delimiters.
#' 
sql2id = function(sql_string){
  stopifnot(is.character(sql_string))
  stopifnot(length(sql_string) == 1)
  
  split = strsplit(sql_string, ".", fixed = TRUE)[[1]]
  sql_id = DBI::Id(remove_delimiters(split, "[]"))
  return(sql_id)
}

## increment file name ---------------------------------------------------- ----
#' Increment number in file path to create a unique file name
#'
#' @param path_and_file_name The path to the file that might need incrementing.
#'
#' @return The file path incremented if necessary to ensure file name is unique.
#' 
#' @details
#' This is a simple mimic of an existing Windows function that will rename
#' `file.csv` to `file (1).csv` or `file (1).csv` to `file (2).csv` if the
#' existing file name already exists.
#' 
increment_file_name = function(path_and_file_name){
  stopifnot(is.character(path_and_file_name))
  
  # return immediately if unique
  if(!file.exists(path_and_file_name)){
    return(path_and_file_name)
  }
  
  ext = tools::file_ext(path_and_file_name)
  pattern = glue::glue("(.* \\()([0-9]+)(\\)\\.{ext})$")
  
  while(file.exists(path_and_file_name)){
    # no number in name
    if(!grepl(pattern, path_and_file_name)){
      path_and_file_name = gsub(glue::glue("\\.{ext}$"), glue::glue(" (1).{ext}"), path_and_file_name)
      next
    }
    
    # number in name
    counter = gsub(pattern, "\\2", path_and_file_name)
    counter = as.numeric(counter) + 1
    path_and_file_name = gsub(pattern, glue::glue("\\1{counter}\\3"), path_and_file_name)
  }
  
  return(path_and_file_name)
}
