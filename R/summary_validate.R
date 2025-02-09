################################################################################
#' Notes
#' 
################################################################################

#' Confirm that control file instructions can be executed.
#'
#' @param control_file a data frame containing summary instructions. Most likely
#' read into memory by `load_control_file`.
#' @param tbl a data frame to summarise. Can be in-memory or remote accessed
#' with dbplyr.
#'
#' @return T/F whether or not all validation checks are passed. Generating
#' warnings for all failed checks.
#'
#' @details
#' The following checks are run and generate a failure if not passed:
#' * required columns exist in data frame
#' * dynamic formula can be executed
#' * Each row has at least one summary generated
#' * columns to sum are numeric
#' 
#' The following checks are run and only generate a warning if not passed:
#' * acceptable column names ("enabled", "group", "label", "distinct", "count",
#'   "sum", "entity", "stddev", "notes")
#' * column is not empty
#' @md
#' 
#' @importFrom  rlang .data
#' @export
validate_summary_control_file = function(control_file, tbl){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.data.frame(tbl))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  tbl_cols = colnames(tbl)
  
  # warn if probably swapped arguments
  if(!("group" %in% ctr_cols) & all(c("group", "enabled") %in% tolower(tbl_cols))){
    warning("Arguments may be out of order. Correct order is control_file, tbl")
  }
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t"))
  }
  
  ## setup for checks ----
  
  # entries of control file (exclude columns: enabled, label, notes)
  tmp = dplyr::select(control_file, -dplyr::starts_with("enabled"), -dplyr::starts_with("label"), -dplyr::starts_with("note"))
  indexes = which(!is.na(control_file), arr.ind = TRUE)
  entries = data.frame(
    row = indexes[,1],
    column = indexes[,2],
    value = control_file[indexes]
  )
  entries$duplicate = duplicated(entries$value)
  entries$is_function = substr(entries$value, 1, 1) == "{"
  entries$column_name = ctr_cols[entries$column]
  entries$is_non_calc = !entries$column_name %in% c("group", "distinct", "count", "sum", "entity", "stddev")
  
  # track passing of checks
  passes_all_critical_checks = TRUE
  
  ## control file column names ----
  
  # acceptable column names in control file
  expected_columns_names = c("enabled", "group", "label", "distinct", "count", "sum", "entity", "stddev", "note", "notes")
  for(cc in ctr_cols){
    if(gsub("[0-9\\.]", "", cc) %in% expected_columns_names){ next }
    
    msg = glue::glue("Control file column {cc} not an accepted name and will be ignored during summarisation.")
    warning(msg)
  }
  
  ## at least one summary per row ----
  
  summary_types = c("distinct", "count", "sum", "entity", "stddev")
  tmp = control_file[,gsub("[0-9]", "", ctr_cols) %in% summary_types, drop = FALSE]
  na_row = apply(is.na(tmp), 1, all)
  
  if(any(na_row)){
    na_row_nums = paste(which(na_row), collapse = ", ")
    
    msg = glue::glue("Found row with no summary: {na_row_nums}")
    warning(msg)
    passes_all_critical_checks = FALSE
  }
  
  ## columns in data frame ----
  
  # required columns exist in data frame
  for(ii in 1:nrow(entries)){
    # pass if duplicate, function, non-calculation, or value in column names
    if(entries$duplicate[ii]){ next }
    if(entries$is_function[ii]){ next }
    if(entries$is_non_calc[ii]){ next }
    if(entries$value[ii] %in% tbl_cols){ next }
    
    num_dupes = sum(entries$duplicate[entries$value == entries$value[ii]])
    msg = glue::glue(
      "Column '{entries$value[ii]}' not found in input table:",
      " (row {entries$row[ii]}, column {entries$column[ii]}).",
      ifelse(num_dupes > 0, " And {num_dupes} other cells.", "")
    )
    warning(msg)
    passes_all_critical_checks = FALSE
  }
  
  ## formula valid when run against data frame ----
  
  # dynamic formula can be executed
  for(ii in 1:nrow(entries)){
    # pass if duplicate or not function
    if(entries$duplicate[ii]){ next }
    if(!entries$is_function[ii]){ next }
    
    formula = entries$value[ii]
    formula = remove_delimiters(formula, "{}")
    
    # potential injection
    possible_injection = !no_obvious_injection(formula)
    if(possible_injection){
      num_dupes = sum(entries$duplicate[entries$value == entries$value[ii]])
      msg = glue::glue(
        "Formula {formula} not tested due to potential code injection:",
        " (row {entries$row[ii]}, column {entries$column[ii]}).",
        ifelse(num_dupes > 0, " And {num_dupes} other cells.", "")
      )
      warning(msg)
      passes_all_critical_checks = FALSE
      next
    }
    
    tryCatch(
      {
        # store current passes
        tmp_pc = passes_all_critical_checks
        passes_all_critical_checks = FALSE
        
        # test code
        tmp = utils::head(tbl, 5)
        tmp = dplyr::mutate(tmp, validating_column = !!rlang::parse_expr(formula))
        tmp = dplyr::collect(tmp)
        
        # restore current pass if code all 
        passes_all_critical_checks = tmp_pc
      },
      error = function(e){
        
        num_dupes = sum(entries$duplicate[entries$value == entries$value[ii]])
        msg = glue::glue(
          "Calculation {entries$value[ii]} errored during testing:",
          " (row {entries$row[ii]}, column {entries$column[ii]}).",
          ifelse(num_dupes > 0, " And {num_dupes} other cells.", ""),
          "\nOriginal error:\n {e}"
        )
        warning(msg)
        
      },
      warning = function(w){
        warning(w)
      }
    )
    
  } # end of loop
  
  ## columns are not empty ----
  
  tmp = dplyr::filter(entries, !.data$duplicate, !.data$is_function, !.data$is_non_calc)
  cols_to_check = unique(tmp$value)
  cols_to_check = cols_to_check[cols_to_check %in% tbl_cols]
  
  count_formula = glue::glue("sum(ifelse(!is.na({cols_to_check}), 1, 0))")
  names(count_formula) = cols_to_check
  
  # get number of missing values
  tmp = dplyr::ungroup(tbl)
  tmp = dplyr::summarise(tmp, num_rows = dplyr::n(), !!!rlang::parse_exprs(count_formula))
  tmp = dplyr::collect(tmp)

  # check
  for(cc in cols_to_check){
    if(tmp[[cc]][1] == 0){
      msg = glue::glue("Column {cc} has only missing values")
      warning(msg)
    }
  }
  
  ## sum of non-numeric columns ----
  
  cols_to_check = dplyr::filter(entries, !.data$is_function & .data$column_name == "sum")
  cols_to_check = unique(cols_to_check$value)
  
  # get example data
  tmp = dplyr::collect(utils::head(tbl, 5))
  
  # check
  for(cc in cols_to_check){
    if(!is.numeric(tmp[[cc]])){
      msg = glue::glue("Attempt to sum non-numeric column '{cc}'")
      warning(msg)
      passes_all_critical_checks = FALSE
    }
  }
  
  ## conclude ----
  return(passes_all_critical_checks)
}
