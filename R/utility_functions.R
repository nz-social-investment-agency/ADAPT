################################################################################
#' Notes
################################################################################

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
