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
#' For each row in the control file, the summary tool produces a summary of tbl
#' according to the specified instructions. All these summaries are combined
#' into a single table and returned.
#' 
#' Validates its control file prior to execution. To validate a control file
#' without execution use `validate_summary_control_file`.
#' 
#' The best way to understand the summary process is to review a worked example.
#' Try `provide_example` for worked examples, or for example control files.
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
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t"))
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
      if(gsub("[0-9]", "", x) != "group"){ return(x) }
      return(c(gsub("group", "grplabel", x), x))
    }
  )
  select_command = unlist(select_command, use.names = FALSE)
  
  ## generate each output ----
  
  results_list = list()
  
  # process each row
  for(ii in 1:nrow(control_file)){
    
    this_row = control_file[ii,]
    
    # group commands
    group_command = this_row[,col_type == "group"]
    group_command = group_command[,!is.na(group_command)]
    group_command = unlist(group_command, use.names = FALSE)
    
    # summary commands
    summary_command = generate_summary_commands(this_row)
    
    # mutate commands - labels
    mutate_inputs = this_row[,col_type %in% c("group", "label")]
    mutate_delim = ifelse(is.na(mutate_inputs), "", "'")
    mutate_command = unlist(mutate_inputs, use.names = FALSE)
    mutate_command = as.character(glue::glue("{mutate_delim}{mutate_command}{mutate_delim}"))
    names(mutate_command) = gsub("group", "grplabel", tolower(colnames(mutate_inputs)))
    
    # mutate commands - empty columns
    mutate_inputs = this_row[,is.na(this_row[1,])]
    mutate_command2 = rep("NA", length(mutate_inputs))
    names(mutate_command2) = names(mutate_inputs)
    mutate_command = c(mutate_command, mutate_command2)
    
    # rename commands
    rename_inputs = this_row[,col_type == "group"]
    rename_inputs = rename_inputs[,!is.na(rename_inputs), drop = FALSE]
    rename_command = unlist(rename_inputs)
    
    # execute
    tmp_results = dplyr::group_by(tbl, !!!rlang::syms(group_command))
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
