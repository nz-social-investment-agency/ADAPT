#' Confirm that control file instructions can be executed.
#'
#' @param control_file a data frame containing summary instructions. Most likely
#' read into memory by `load_control_file`.
#' @param tbl a data frame to summarise. Can be in-memory or remote accessed
#' with dbplyr.
#' @param save_file specify the file to save results to. Optional, overrides the
#' FILE column of the control file if provided.
#' @param partial_output T/F whether or not to skip the check that each output 
#' file must have either all summaries enabled or all disabled. Defaults to
#' FALSE to avoid confusion if files are overwritten.
#' 
#' @return T/F whether or not all validation checks are passed. Generating
#' warnings for all failed checks.
#'
#' @details
#' The following checks are run and generate a failure if not passed:
#' * The column 'file' exists
#' * Required columns exist in data frame
#' * Dynamic formula contain `no_obvious_escaping_injection`
#' * Dynamic formula can be executed
#' * Each row has at least one summary generated
#' * Columns to sum are numeric
#' * Grouping columns are not dynamic
#' * Grouping columns are unique in each row
#' * For each output file, either all summaries (rows) are enable or all are
#'   disabled (skipped if `partial_output` is `TRUE`)
#' 
#' The following checks are run and only generate a warning if not passed:
#' * acceptable column names ("enabled", "group", "label", "distinct", "count",
#'   "sum", "entity", "stddev", "max", "min", "seed", "where", "notes")
#' * column is not empty
#' * grouping columns not used for summarising
#' @md
#' 
#' @importFrom  rlang .data
#' @export
validate_summary_control_file = function(control_file, tbl, save_file = NA_character_, partial_output = FALSE){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.data.frame(tbl) | dplyr::is.tbl(tbl))
  stopifnot(is.character(save_file))
  stopifnot(is.na(save_file) | dir.exists(dirname(save_file)))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  tbl = tolower_colnames(tbl)
  tbl_cols = colnames(tbl)
  
  # control file cells to lower case
  control_file = tolower_control_file_cells(control_file, colnames(tbl))
  
  # warn if probably swapped arguments
  if(!("group" %in% ctr_cols) & all(c("group", "enabled") %in% tolower(tbl_cols))){
    warning("Arguments may be out of order. Correct order is control_file, tbl")
  }
  
  if(!is.na(save_file)){
    control_file$file = save_file
  }
  
  # remove delimiters []
  for(cc in ctr_cols){
    control_file[[cc]] = remove_delimiters(control_file[[cc]], "[]")
  }
  
  # filter to enabled summaries
  unfiltered_cf = control_file
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  ## setup for checks ----
  
  # entries of control file (exclude columns: enabled, label, notes)
  indexes = which(!is.na(control_file), arr.ind = TRUE)
  entries = data.frame(
    row = indexes[,1],
    column = indexes[,2],
    value = control_file[indexes]
  )
  entries$column_name = ctr_cols[entries$column]
  entries = dplyr::filter(entries, .data$column_name %not_in% c("enabled", "file", "label", "note"))
  entries$duplicate = duplicated(entries$value)
  entries$is_function = substr(trimws(entries$value), 1, 1) == "{"
  
  calc_cols = c("group", "distinct", "count", "sum", "entity", "stddev", "max", "min", "seed")
  entries$is_non_calc = !grepl(paste0("^", calc_cols, collapse = "|"), entries$column_name)
  
  # union_all for handling entity__min and entity__max required
  entity_union_all_req = FALSE
  # track passing of checks
  passes_all_critical_checks = TRUE
  
  ## control file column names ----
  
  # acceptable column names in control file
  expected_columns_names = c(
    "enabled", "file", "group", "label", "distinct", "count", "sum", "entity", "stddev",
    "max", "min", "seed", "where", "note", "notes","start_time","end_time","status"
  )
  for(cc in ctr_cols){
    if(gsub("[0-9\\.]", "", cc) %in% expected_columns_names){ next }
    
    msg = glue::glue("Control file column '{cc}' not an accepted name and will be ignored during summarisation.")
    warning(msg)
  }
  
  # file in column names
  if("file" %not_in% ctr_cols & is.na(save_file)){
    warning("Either 'save_file' input must be provided or control file must have a column 'File' for where to write output.")
    passes_all_critical_checks = FALSE
  }
  
  ## at least one summary per row ----
  
  summary_types = c("distinct", "count", "sum", "entity", "stddev", "max", "min", "seed")
  tmp = dplyr::select(control_file, dplyr::starts_with(summary_types))
  na_row = apply(is.na(tmp), 1, all)
  
  if(any(na_row)){
    na_row_nums = paste(which(na_row), collapse = ", ")
    
    msg = glue::glue("Found row with no summary: {na_row_nums}")
    warning(msg)
    passes_all_critical_checks = FALSE
  }
  
  ## columns in data frame ----
  
  # required columns exist in data frame
  for(ii in seq_len(nrow(entries))){
    # pass if duplicate, function, non-calculation, or value in column names
    if(entries$duplicate[ii]){ next }
    if(entries$is_function[ii]){ next }
    if(entries$is_non_calc[ii]){ next }
    if(entries$value[ii] %in% tbl_cols){ next }
    
    # entity
    if(grepl("^entity", entries$column_name[ii])){
      min_ent = paste0(entries$value[ii], "__min")
      max_ent = paste0(entries$value[ii], "__max")
      if(any(c(min_ent, max_ent) %in% tbl_cols)){
        entity_union_all_req = TRUE
        next
      }
    }
    
    num_dupes = sum(entries$duplicate[entries$value == entries$value[ii]])
    msg = glue::glue(
      "Column '{entries$value[ii]}' not found in input table:",
      " (row {entries$row[ii]}, column {entries$column[ii]}).",
      ifelse(num_dupes > 0, " And {num_dupes} other cells.", ""),
      ifelse(grepl("^entity", entries$column_name[ii]), " *__min or *__max not found either.", "")
    )
    warning(msg)
    passes_all_critical_checks = FALSE
  }
  
  ## group columns are not dynamic ----
  
  # required columns exist in data frame
  for(ii in seq_len(nrow(entries))){
    # pass if duplicate, non-group, non-function
    if(entries$duplicate[ii]){ next }
    if(!grepl("^group", entries$column_name[ii])){ next }
    if(!entries$is_function[ii]){ next }
    
    num_dupes = sum(entries$duplicate[entries$value == entries$value[ii]])
    msg = glue::glue(
      "Group '{entries$value[ii]}' not accepted as groups may not be dynamic:",
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
    possible_injection = !no_obvious_escaping_injection(formula)
    if(possible_injection){
      num_dupes = sum(entries$duplicate[entries$value == entries$value[ii]])
      msg = glue::glue(
        "Formula {formula} not tested due to potential escaping code injection:",
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
        # where >> filter, else >> mutate
        if(grepl("^where", entries$column_name[ii])){
          tmp = dplyr::filter(tmp, !!rlang::parse_expr(formula))
        } else {
          tmp = dplyr::mutate(tmp, validating_column = !!rlang::parse_expr(formula))
          tmp = dplyr::select(tmp, "validating_column")
        }
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
      warning = function(w){ warning(w) }
    )
    
  } # end of loop
  
  ## columns are not empty ----
  
  tmp = dplyr::filter(entries, !.data$duplicate, !.data$is_function, !.data$is_non_calc)
  cols_to_check = unique(tmp$value)
  cols_to_check = cols_to_check[cols_to_check %in% tbl_cols]
  
  # check
  for(cc in cols_to_check){
    tmp = dplyr::filter(tbl, !is.na(.data[[cc]]))
    tmp = dplyr::select(tmp, dplyr::all_of(cc))
    tmp = utils::head(tmp, 1)
    tmp = dplyr::collect(tmp)
    
    if(nrow(tmp) == 0){
      msg = glue::glue("Column {cc} has only missing values")
      message(msg)
    }
  }
  
  ## sum of non-numeric columns ----
  
  cols_to_check = dplyr::filter(
    entries,
    !.data$is_function,
    grepl("^sum", .data$column_name) | grepl("^seed", .data$column_name)
  )
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
  
  ## grouping columns not used for summarizing ----
  
  # grouping entities
  grp_entities = dplyr::filter(entries, grepl("^group", .data$column_name))
  # summary entities
  sum_entities = dplyr::filter(entries, !.data$is_non_calc & !grepl("^group", .data$column_name))
  
  for(cc in tbl_cols){
    # skip if no need for union_all
    if(!entity_union_all_req){ next }
    
    rows_used_for_grouping = dplyr::filter(grp_entities, grepl(paste0("\\b",cc,"\\b"), .data$value))
    rows_used_for_grouping = unique(dplyr::pull(rows_used_for_grouping, "row"))
    
    rows_used_for_summary = dplyr::filter(sum_entities, grepl(paste0("\\b",cc,"\\b"), .data$value))
    rows_used_for_summary = unique(dplyr::pull(rows_used_for_summary, "row"))
    
    # skip if no row has both
    rows_use_for_both = base::intersect(rows_used_for_grouping, rows_used_for_summary)
    if(length(rows_use_for_both) == 0){ next }
    rows_use_for_both = paste0(rows_use_for_both, collapse = ", ")
    
    # warn on both
    msg = glue::glue(
      "Column '{cc}' is used for grouping and summarising in row(s) {rows_use_for_both}.",
      " Due to how entities are handled this risks double counting in your results.")
    warning(msg)
  }
  
  ## grouping columns are unique for each row ----
  
  # grouping entities
  grp_entities = dplyr::filter(entries, grepl("^group", .data$column_name))
  grp_entities = dplyr::group_by(grp_entities, .data$row, .data$value)
  grp_entities = dplyr::summarise(grp_entities, num = dplyr::n(), .groups = "drop")
  grp_entities = dplyr::filter(grp_entities, .data$num > 1)
  
  passes_all_critical_checks = passes_all_critical_checks & nrow(grp_entities) == 0
  
  for(ii in seq_len(nrow(grp_entities))){
    # warn on both
    msg = glue::glue(
      "Column '{grp_entities$value[ii]}' is used more than once for grouping.",
      "See enabled row '{grp_entities$row[ii]}'.")
    warning(msg)
  }
  
  ## each file is all enabled or all disabled ----
  
  if(!partial_output && all(c("enabled", "file") %in% colnames(unfiltered_cf))){
    unfiltered_cf$enabled = tolower(unfiltered_cf$enabled) %in% c("true", "1", "t", "yes", "y")
    unfiltered_cf = dplyr::group_by(unfiltered_cf, file)
    unfiltered_cf = dplyr::summarise(unfiltered_cf, num_state = dplyr::n_distinct(.data$enabled))
    unfiltered_cf = dplyr::filter(unfiltered_cf, .data$num_state > 1)
    
    if(nrow(unfiltered_cf) > 0){
      problem_files = paste(unfiltered_cf$file, collapse = ", ")
      msg = glue::glue("Detected files with both enabled and disabled summaries/rows: {problem_files}")
      warning(msg)
      passes_all_critical_checks = FALSE
    }
  }
  
  ## conclude ----
  return(passes_all_critical_checks)
}
