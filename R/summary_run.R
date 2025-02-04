################################################################################
#' Notes
#' - awaiting helper documentation
#' - awaiting tests
#' 
################################################################################

#'
#'
#'
#'

run_summary = function(control_file, tbl, remote_na_from_groups = TRUE){
  
  ## initialize ----
  
  stopifnot(is.data.frame(control_file))
  stopifnot(is.data.frame(tbl))
  
  ctr_cols = trimws(tolower(colnames(control_file)))
  colnames(control_file) = ctr_cols
  tbl_cols = colnames(tbl)
  
  valid_control_file = validate_summary_control_file(control_file, tbl)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    control_file = dplyr::filter(control_file, tolower(enabled) %in% c("true", "1", "t"))
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
    mutate_commands = unlist(mutate_inputs, use.names = FALSE)
    names(mutate_commands) = gsub("group", "grplabel", tolower(colnames(mutate_inputs)))
    
    # execute
    tmp_results = dplyr::group_by(tbl, !!!rlang::syms(group_command))
    tmp_results = dplyr::summarise(tmp_results, !!!rlang::parse_exprs(summary_command))
    tmp_results = dplyr::ungroup(tmp_results)
    tmp_results = dplyr::mutate(tmp_results, !!!rlang::parse_exprs(mutate_commands))
    tmp_results = dplyr::select(tmp_results, dplyr::any_of(select_command))
    
    # write out SQL
    # if(debug == TRUE & "sql" %in% class(tbl)){
    #   
    # }
    
    # fetch results
    this_df = collect(tmp_results)
    # add to output
    results_list = c(results_list, list(this_df))
  }
  
  ## conclude ----
  results_df = dplyr::bind_rows(results_list)
  
  
  return(results_df)
}
