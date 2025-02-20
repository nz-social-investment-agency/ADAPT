################################################################################
#' Notes
#' 
################################################################################

#' Execute summary tool.
#' 
#' @param control_file a data frame containing summary instructions. Most likely
#' read into memory by `load_control_file`.
#' @param tbl a data frame to summarise. Can be in-memory or remote accessed
#' with dbplyr.
#' @param remove_na_from_groups T/F whether missing values from grouping columns
#' should be excluded from the results. Defaults to TRUE.
#' @param debug_folder an existing folder where debug information should be
#' written to disc. If NA (the default) not debug information is written.
#' 
#' @return A data frame from executing the summary defined by each row of the
#' control file and appending all the summaries together.
#' 
#' @details
#' The best way to understand the summary process is to review a worked example.
#' Try `provide_example` for worked examples, or for example control files.
#' 
#' For each row in the control file, the summary tool produces a summary of tbl
#' according to the specified instructions. All these summaries are combined
#' into a single table and returned.
#' 
#' Control files are validated prior to execution. To validate a control file
#' without execution use `validate_summary_control_file`.
#' 
#' The accepted columns for the control file are:
#' * ENABLED - TRUE/FALSE - If this column is included then rows set to FALSE
#' will be omitted. Except for ENABLED, you can have any number of columns of
#' each type.
#' * LABEL - free text - allows you to include arbitrary text in your output,
#' this is intended to provide a description of the summary.
#' * GROUP - name of columns of `tbl` - When the summary for the row is
#' produced, the output is grouped by the group columns.
#' * DISTINCT - name of columns of `tbl` - counts the number of distinct values
#' in the column.
#' * COUNT - name of columns of `tbl` - counts the number of non-missing values
#' in the column.
#' * SUM - name of columns of `tbl` - calculates the sum total of the column,
#' during validation the tool will error is this column is not numeric.
#' * STDDEV - name of columns of `tbl` - calculates the standard deviation of the
#' column.
#' * ENTITY  - name of columns of `tbl` -  similar to DISTINCT, but checks for
#' columns with `*__min` and `*__max` suffixes and takes a distinct over the union
#' of these columns if available. Designed for counting entities.
#' * NOTES - free text - column is ignored and does not effect output. Intended
#' for adding notes to control file. Any other column names are also ignored,
#' but generate a warning.
#' 
#' Anywhere you can give the name of a column of `tbl` in the control file,
#' you can instead give input in \{curry brackets\}. This input is treated as R
#' code. If `tbl` is a remote data frame, then this will be translated to SQL
#' using dbplyr.
#' 
#' The intended use of this feature is for making minor adjustments to
#' variables. For example replacing missing values.
#' @md
#' 
#' @importFrom  rlang .data
#' @export
run_summary = function(control_file, tbl, remove_na_from_groups = TRUE, debug_folder = NA_character_){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.data.frame(tbl))
  stopifnot(is.logical(remove_na_from_groups))
  stopifnot(is.character(debug_folder))
  stopifnot(is.na(debug_folder) | dir.exists(debug_folder) )
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  tbl_cols = colnames(tbl)
  
  valid_control_file = validate_summary_control_file(control_file, tbl)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  if(nrow(control_file) == 0){
    warning("All rows of control file disabled, returnig NULL")
    return(NULL)
  }
  
  ## setup for generation ----
  
  col_type = gsub("[0-9\\.]", "", ctr_cols)
  
  # select commands
  output_types = c("group", "label", "distinct", "count", "sum", "entity", "stddev")
  select_command = tolower(ctr_cols)
  select_command = select_command[col_type %in% output_types]
  # insert 'grplabel' for each group
  select_command = lapply(
    select_command,
    function(x){
      if(gsub("[0-9\\.]", "", x) != "group"){ return(x) }
      return(c(gsub("group", "grplabel", x), x))
    }
  )
  select_command = unlist(select_command, use.names = FALSE)
  
  ## generate each output ----
  
  results_list = list()
  
  # process each row
  for(ii in 1:nrow(control_file)){
    
    this_row = control_file[ii,]
    
    # union all conversion for entities
    entity_tbl = entity_union_all_conversion(this_row, tbl)
    
    # group commands
    group_command = this_row[,col_type == "group", drop = FALSE]
    group_command = group_command[,!is.na(group_command), drop = FALSE]
    group_command = unlist(group_command, use.names = FALSE)
    
    # summary commands
    summary_command = generate_summary_commands(this_row)
    
    # mutate commands - labels
    mutate_inputs = this_row[,col_type %in% c("group", "label"), drop = FALSE]
    mutate_delim = ifelse(is.na(mutate_inputs), "", "'")
    mutate_command = unlist(mutate_inputs, use.names = FALSE)
    mutate_command = as.character(glue::glue("{mutate_delim}{mutate_command}{mutate_delim}"))
    names(mutate_command) = gsub("group", "grplabel", tolower(colnames(mutate_inputs)))
    
    # mutate commands - empty columns
    mutate_inputs = this_row[,is.na(this_row[1,]), drop = FALSE]
    mutate_command2 = rep("NA", length(mutate_inputs))
    names(mutate_command2) = names(mutate_inputs)
    mutate_command = c(mutate_command, mutate_command2)
    
    # rename commands
    rename_inputs = this_row[,col_type == "group", drop = FALSE]
    rename_inputs = rename_inputs[,!is.na(rename_inputs), drop = FALSE]
    rename_command = unlist(rename_inputs)
    
    # execute
    tmp_results = dplyr::group_by(entity_tbl, !!!rlang::syms(group_command))
    tmp_results = dplyr::summarise(tmp_results, !!!rlang::parse_exprs(summary_command), .groups = "drop")
    tmp_results = dplyr::mutate(tmp_results, !!!rlang::parse_exprs(mutate_command))
    tmp_results = dplyr::rename(tmp_results, !!!rlang::parse_exprs(rename_command))
    tmp_results = dplyr::select(tmp_results, dplyr::all_of(select_command))
    
    # write out for debug
    if(!is.na(debug_folder)){
      # remote SQL table
      if(dbplyr::is.sql(tmp_results)){
        save_code_to_script(dplyr::show_query(tmp_results), "summary.sql", debug_folder)
        next  
      }
      # otherwise local data frame
      tmp = glue::glue(
        "\nGROUP\n",
        paste(group_command, collapse = "\n"),
        "\n\nSUMMARISE\n",
        paste(names(summary_command),"=",summary_command, collapse = "\n"),
        "\n\nRENAME\n",
        paste(names(rename_command),"=",rename_command, collapse = "\n"),
        "\n\nMUTATE\n",
        paste(names(mutate_command),"=",mutate_command, collapse = "\n")
      )
      save_code_to_script(tmp, "summary.R", debug_folder)
    }
    
    # fetch results
    this_df = dplyr::collect(tmp_results)
    # add to output
    results_list = c(results_list, list(this_df))
  }
  
  ## combine ----
  
  # ensure label and group columns are character
  results_list = lapply(
    results_list,
    function(df){
      text_cols = colnames(df)
      text_cols = text_cols[grepl("^(grplabel|group|label)", text_cols, ignore.case = TRUE)]
      for(cc in text_cols){
        df[[cc]] = as.character(df[[cc]])
      }
      return(df)
    }
  )
  
  results_df = dplyr::bind_rows(results_list)
  
  ## filter our NAs if required ----
  if(remove_na_from_groups){
    group_cols = colnames(results_df)
    group_cols = group_cols[grepl("^group", group_cols, ignore.case = TRUE)]
    group_suffix = gsub("^group", "", group_cols)
    
    # only discard rows where grplabel is not NA but group is NA
    # so keep where grplabel is NA or group is not NA
    filter_commands = glue::glue("(is.na(grplabel{group_suffix}) | !is.na(group{group_suffix}))")
    
    results_df = dplyr::filter(results_df, !!!rlang::parse_exprs(filter_commands))
  }
  
  ## conclude ----
  results_df = dplyr::select(results_df, dplyr::all_of(select_command))
  return(results_df)
}
