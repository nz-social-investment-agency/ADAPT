################################################################################
#' Notes
#' 
################################################################################

## Extract components of suppression instructions ------------------------- ----
#' Decompose suppression instructions into individual components and validate
#' correctness of instructions.
#'
#' @param string a text string with the suppression instructions to process.
#'
#' @return a list with five components:
#' * `input` the original input string.
#' * `column` the column to process.
#' * `sign` the inequality that defines suppression.
#' * `threshold` the numeric value to suppress below.
#' * `valid` T/F whether the decomposed input makes for valid suppression rules.
#' @md
#'
#' @details
#' Splits an input string into three components and then evaluates whether these
#' components make valid suppression instructions.
#' 
#' For example: "colname < 6" will be decomposed into column = "colname", sign
#' = "<", and threshold = "6".
#' 
suppression_format_extract = function(string){
  stopifnot(is.character(string))
  
  # remove spaces
  string = gsub(" ", "", string)
  
  # sign = string with all non-<=> characters removed
  sign = gsub("[^<=>]", "", string)
  
  # column and threshold are on either size of sign
  split = strsplit(string, sign)[[1]]
  if(grepl("<", sign)){
    column = split[1]
    threshold = split[2]
  } else if(grepl(">", sign)) {
    column = split[2]
    threshold = split[1]
  } else {
    column = NA_character_
    threshold = NA_character_
  }
  threshold = suppressWarnings(as.numeric(threshold))
  
  # check validity
  valid = sign %in% c("<", ">", "<=", ">=") &
    !is.na(threshold) &
    nchar(column) >= 1 
  
  # return
  return(list(input = string, column = column, sign = sign, threshold = threshold, valid = valid))
}
