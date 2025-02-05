################################################################################
#' Notes
#' - awaiting helper documentation
#' - awaiting tests
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
#' The following checks are run and generate a failure if not passed:
#' * required columns exist in data frame
#' * dynamic formula can be executed
#' * Each row has at least one summary generated
#' 
#' The following checks are run and only generate a warning if not passed:
#' * acceptable column names ("enabled", "group", "label", "distinct", "count",
#'   "sum", "entity", "stddev", "notes")
#' * column is not empty
#' * columns to sum are numeric
#' @md
#'
#' @importFrom  rlang .data
#' @export
run_summary = function(control_file, tbl, remove_na_from_groups = TRUE, debug_folder = NA_character_){
  stopifnot(is.data.frame(control_file))
  stopifnot(is.data.frame(tbl))
  stopifnot(is.logical(remove_na_from_groups))
  stopifnot(is.character(debug))
  stopifnot(is.na(debug_folder) | dir.exists(debug_folder) )
  
  ## initialize ----
  
  # # standardize column names ### requires a more general solution to work across all control files
  # cols = colnames(file_contents)
  # cols = gsub("[0-9\\.]", "", cols)
  # cols = sapply(1:length(cols), function(ii){paste0(cols[ii], sum(cols[ii] == cols[1:ii]))})
  # colnames(file_contents) = cols
  # 
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  tbl_cols = colnames(tbl)
  
  valid_control_file = validate_summary_control_file(control_file, tbl)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(.data$enabled) %in% c("true", "1", "t"))
  }
  
  ## setup for generation ----
  
  results_list = list()
  
  col_type = gsub("[0-9]", "", tbl_cols)
  
  # select commands
  output_types = c("group", "label", "distinct", "count", "sum", "entity", "stddev")
  select_command = tolower(tbl_cols)
  select_command = select_command[col_type %in% output_types]
  # insert 'val' for each group
  select_command = lapply(
    select_command,
    function(x){
      if(gsub("[0-9]", "", x) != "group"){ return(x) }
      return(c(gsub("group", "grplabel", x), x))
    }
  )
  select_command = unlist(select_command, use.names = FALSE)
  
  ## generate each output ----
  
  # process each row
  for(ii in 1:nrow(control_file)){
    
    this_row = unlist(control_file[ii,])
    
    # group commands
    group_command = this_row[,col_type == "GROUP"]
    group_command = group_command[,!is.na(group_command)]
    
    # summary commands
    summary_command = generate_summary_commands(this_row)
    
    # mutate commands
    mutate_inputs = this_row[,col_type %in% c("GROUP", "LABEL")]
    mutate_command = unlist(mutate_inputs, use.names = FALSE)
    names(mutate_command) = gsub("group", "grplabel", tolower(colnames(mutate_inputs)))
    
    # execute
    tmp_results = dplyr::group_by(tbl, !!!rlang::syms(group_command))
    tmp_results = dplyr::summarise(tmp_results, !!!rlang::parse_exprs(summary_command))
    tmp_results = dplyr::ungroup(tmp_results)
    tmp_results = dplyr::mutate(tmp_results, !!!rlang::parse_exprs(mutate_command))
    tmp_results = dplyr::select(tmp_results, dplyr::any_of(select_command))
    
    # write out for debug
    if(!is.na(debug_folder)){
      # remote SQL table
      if(dbplyr::is.sql(tmp_results)){
        save_code_to_script(dplyr::show_query(tmp_results), "summary.sql", debug_folder)
        next  
      }
      # otherwise local data frame
      tmp = glue::glue("GROUP\n{group_command}\n\nSUMMARISE\n{summary_command}\n\nMUTATE\n{mutate_command}")
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
    
    filter_commands = glue::glue("!is.na({group_cols})")
    
    results_df = dplyr::filter(results_df, !!!parse_exprs(filter_commands))
  }
  
  ## conclude ----
  return(results_df)
}
