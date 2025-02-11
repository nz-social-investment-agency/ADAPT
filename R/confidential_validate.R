################################################################################
#' Notes
#' 
################################################################################

#' Confirm that control file instructions can be executed.
#'
#' @param control_file a data frame containing confidentialisation instructions.
#' Most likely read into memory by `load_control_file`.
#' @param tbl a data frame to confidentialise. Should be in local R memory.
#'
#' @return T/F whether or not all validation checks are passed. Generating
#' warnings for all failed checks.
#'
#' @details
#' The following checks are run and generate a failure if not passed:
#' * First column of `control_file` contains confidentiality commands
#' * Remaining columns listed in `control_file` match the column names of `tbl`.
#' * At most one row of DROP, RENAME, ROUND, and MISSING_TO instructions.
#' * Rounding instructions are of the accepted types (RR3, GRR, CONV10, CONV100,
#' CONV1000).
#' * Missing NA value options are numeric.
#' * Suppression rules match the accepted pattern (column_name < number value).
#' @md
#' 
#' @export
validate_confidential_control_file = function(control_file, tbl){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.data.frame(tbl))
  
  ## initialize ----
  
  conf_cmds = tolower(control_file[,1])
  ncols = ncol(control_file)
  
  ## setup for checks ----
  
  # simple function to handle unaccepted entries
  handle_unaccepted = function(unaccepted_entries, msg, pass_all_checks){
    # no change if nothing unacceptable
    if(length(unaccepted_entries) == 0){
      return(pass_all_checks)
    }
    # prepare message
    unaccepted_entries = sort(unaccepted_entries)
    unaccepted_entries = unaccepted_entries[1:min(length(unaccepted_entries), 3)]
    unaccepted_entries = paste0("'", unaccepted_entries, "'", collapse = ", ")
    warning(glue::glue(msg))
    # return failure state
    return(FALSE)
  }
  
  # track passing of checks
  pass_all_checks = TRUE
  
  ## control file command column ----
  
  first_column = control_file[,1]
  
  accepted_commands = c("drop", "rename", "missing_to", "round", "suppress", "notes", "note")
  unaccepted_entries = first_column[tolower(first_column) %not_in% accepted_commands]
  unaccepted_entries = unique(unaccepted_entries)
  
  msg = "Column 1 contains inputs {unaccepted_entries} that are not accepted"
  pass_all_checks = handle_unaccepted(unaccepted_entries, msg, pass_all_checks)
  
  ## control file column names ----
  
  required_column_names = colnames(control_file)
  required_column_names = required_column_names[2:length(required_column_names)]
  
  unaccepted_entries = required_column_names[required_column_names %not_in% colnames(tbl)]
  
  msg = "Columns {unaccepted_entries} not found in tbl"
  pass_all_checks = handle_unaccepted(unaccepted_entries, msg, pass_all_checks)
  
  ## command limits ----
  
  first_column = tolower(control_file[,1])
  single_row_commands = c("drop", "rename", "round", "missing_to")
  
  for(ss in single_row_commands){
    if(sum(first_column == ss, na.rm = TRUE) <= 1){ next }
    msg = glue::glue("Limit of one {toupper(ss)} row in control_file")
    warning(msg)
    pass_all_checks = FALSE
  }
  
  ## valid rounding instructions ----
  
  rounding_row = control_file[conf_cmds == "round",2:ncols]
  rounding_row = rounding_row[!is.na(rounding_row)]
  
  accepted_commands = c("RR3", "GRR", "CONV10", "CONV100", "CONV1000")
  unaccepted_entries = rounding_row[tolower(rounding_row) %not_in% tolower(accepted_commands)]
  unaccepted_entries = unique(unaccepted_entries)
  
  msg = "Rounding instructions contain inputs {unaccepted_entries} that are not accepted"
  pass_all_checks = handle_unaccepted(unaccepted_entries, msg, pass_all_checks)
  
  ## numeric alternatives to NAs ----
  
  missing_to_row = control_file[conf_cmds == "missing_to",2:ncols]
  missing_to_row = missing_to_row[!is.na(missing_to_row)]
  
  non_numeric = is.na(suppressWarnings(as.numeric(missing_to_row)))
  unaccepted_entries = missing_to_row[non_numeric]
  unaccepted_entries = unique(unaccepted_entries)
  
  msg = "Non-numeric values used to replace NA during confidentialisation: {unaccepted_entries}"
  pass_all_checks = handle_unaccepted(unaccepted_entries, msg, pass_all_checks)
  
  ## valid suppression rules ----
  
  # setup
  suppress_entries = control_file[conf_cmds == "suppress",2:ncols]
  suppress_entries = suppress_entries[!is.na(suppress_entries)]
  suppress_entries = unique(suppress_entries)
  
  suppress_entries_df = lapply(suppress_entries, suppression_format_extract)
  suppress_entries_df = dplyr::bind_rows(c(
    suppress_entries_df,
    list(list(input = character(), column = character(), sign = character(), threshold = numeric(), valid = logical()))
  ))
  
  # valid pattern
  unaccepted_entries = suppress_entries_df$input[!suppress_entries_df$valid]
  unaccepted_entries = unique(unaccepted_entries)
  
  msg = "Suppression instructions contain inputs {unaccepted_entries} that are not accepted"
  pass_all_checks = handle_unaccepted(unaccepted_entries, msg, pass_all_checks)
  
  # column names
  rename_entries = control_file[conf_cmds == "rename",2:ncols]
  rename_entries = rename_entries[!is.na(rename_entries)]
  
  available_columns = c(rename_entries, colnames(tbl))
  entry_to_check = suppress_entries_df$valid & suppress_entries_df$column %not_in% available_columns
  unaccepted_entries = suppress_entries_df$column[entry_to_check]
  unaccepted_entries = unique(unaccepted_entries)
  
  msg = "Suppression instructions require column {unaccepted_entries} that do not exist"
  pass_all_checks = handle_unaccepted(unaccepted_entries, msg, pass_all_checks)
  
  ## conclude ----
  return(pass_all_checks)
}
