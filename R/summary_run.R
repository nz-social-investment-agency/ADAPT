#' Execute summary tool.
#' 
#' @param control_file location of the control file containing summary
#' instructions to read into R. Accepted `.csv` and `.xlsx` file formats.
#' @param sheet Sheet to read if control file is `.xlsx` format. As per
#' `openxlsx2::read_xlsx`: either a string (name of a sheet), or an integer
#' (the position of the sheet). Defaults to the first sheet otherwise.
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
#' * FILE - file path and name - where the summary output should be saved,
#' output can be divided across multiple files. Files and folders will be
#' created if they do not exist, and overwritten if they do exist.
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
run_summary = function(control_file, sheet = NULL, tbl, remove_na_from_groups = TRUE, debug_folder = NA_character_){
  stopifnot(is.character(control_file), file.exists(control_file))
  stopifnot(is.null(sheet) | is.character(sheet))
  stopifnot(is.data.frame(tbl) | dplyr::is.tbl(tbl))
  stopifnot(is.logical(remove_na_from_groups))
  stopifnot(is.character(debug_folder))
  stopifnot(is.na(debug_folder) | dir.exists(debug_folder) )
  
  run_time_inform_user("Summary tool initiated.")
  
  ## load control file ----
  
  loaded_cf = load_control_file(control_file, sheet = sheet)
  # drop progress reporting columns
  loaded_cf = dplyr::select(loaded_cf, -dplyr::any_of(c("start_time", "end_time", "status")))
  
  ## initialize ----
  
  ctr_cols = trimws(tolower(colnames(loaded_cf)))
  colnames(loaded_cf) = ctr_cols
  tbl_cols = colnames(tbl)
  is_sql = any(grepl("sql", class(tbl), ignore.case = TRUE))
  tbl = tolower_colnames(tbl)
  
  # control file columns to lower case
  loaded_cf = tolower_control_file_cells(loaded_cf, colnames(tbl))
  
  valid_control_file = validate_summary_control_file(loaded_cf, tbl)
  stopifnot(valid_control_file)
  
  # filter to enabled summaries
  if("enabled" %in% ctr_cols){
    loaded_cf = dplyr::filter(loaded_cf, tolower(.data$enabled) %in% c("true", "1", "t", "yes", "y"))
  }
  
  if(nrow(loaded_cf) == 0){
    warning("All rows of control file disabled, returnig NULL")
    return(NULL)
  }
  
  # file path fix
  loaded_cf$file = adjust_file_path_handling(loaded_cf$file)
  
  result_df = dplyr::mutate(
    loaded_cf,
    start_time = NA_character_,
    end_time = NA_character_,
    status = NA_character_
  )
  on.exit(expr = {
    save_control_file_w_progress(control_file, sheet = sheet, result_df)
  }, add = TRUE, after = TRUE)
  
  ## remove existing files ----
  
  for(ff in unique(loaded_cf$file)){
    if(file.exists(ff)){
      unlink(ff)
    }
  }
  
  ## setup for generation ----
  
  col_type = gsub("[0-9\\.]", "", ctr_cols)
  
  # select commands
  output_types = c("group", "label", "distinct", "count", "sum", "entity", "stddev")
  select_command = ctr_cols[col_type %in% output_types]
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
  
  # process each row
  for(ii in seq_len(nrow(loaded_cf))){
    
    this_row = dplyr::slice(loaded_cf, ii)

    start_time = as.character(Sys.time())
    msg = sprintf("Summary step %3d of %d", ii, nrow(loaded_cf))
    run_time_inform_user(msg)

    ## prepare commands ----
    
    # union all conversion for entities
    entity_tbl = entity_union_all_conversion(this_row, tbl)
    
    # filter commands
    filter_command = dplyr::select(this_row, dplyr::starts_with("where"))
    filter_command = unlist(filter_command, use.names = FALSE)
    filter_command = filter_command[!is.na(filter_command)]
    if(is.null(filter_command)){ filter_command = character() }
    filter_command = remove_delimiters(filter_command, "{}")
    
    # group commands
    group_command = dplyr::select(this_row, dplyr::starts_with("group"))
    group_command = unlist(group_command, use.names = FALSE)
    group_command = group_command[!is.na(group_command), drop = FALSE]
    
    
    # summary commands
    summary_command = generate_summary_commands(this_row, is_sql)
    
    # mutate commands - labels
    mutate_inputs = dplyr::select(this_row, dplyr::starts_with(c("group", "label")))
    mutate_command = unlist(mutate_inputs, use.names = FALSE)
    mutate_delim = ifelse(is.na(mutate_inputs), "", "'")
    mutate_command = as.character(glue::glue("{mutate_delim}{mutate_command}{mutate_delim}"))
    names(mutate_command) = gsub("group", "grplabel", tolower(colnames(mutate_inputs)))
    
    # mutate commands - empty columns
    mutate_inputs = this_row[,is.na(this_row[1,]), drop = FALSE]
    mutate_command2 = rep("NA", length(mutate_inputs))
    names(mutate_command2) = names(mutate_inputs)
    mutate_command = c(mutate_command, mutate_command2)
    
    # rename commands
    rename_inputs = dplyr::select(this_row, dplyr::starts_with("group"))
    rename_inputs = rename_inputs[,!is.na(rename_inputs), drop = FALSE]
    rename_command = unlist(rename_inputs)
    
    ## execute commands ----
    
    # execute, capturing messages
    status = tryCatch(
      {
        tmp_results = dplyr::filter(entity_tbl, !!!rlang::parse_exprs(filter_command))
        tmp_results = dplyr::group_by(tmp_results, !!!rlang::syms(group_command))
        tmp_results = dplyr::summarise(tmp_results, !!!rlang::parse_exprs(summary_command), .groups = "drop")
        tmp_results = dplyr::mutate(tmp_results, !!!rlang::parse_exprs(mutate_command))
        tmp_results = dplyr::rename(tmp_results, !!!rlang::parse_exprs(rename_command))
        tmp_results = dplyr::select(tmp_results, dplyr::all_of(select_command))
        
        if(is_sql){
          sql_query = dbplyr::sql_render(tmp_results)
        }
        # fetch results
        this_df = dplyr::collect(tmp_results)
        
        "Successful completion"
      },
      error = function(e){
        msg = paste(e$message, collapse = "\n")
        msg = glue::glue("Stopped with error: ", msg)
        return(msg)
      }
    )
    
    ## debug write ----
    
    # write out for debug
    if(!is.na(debug_folder)){
      tmp = glue::glue(
        "\nFILTER\n",
        paste(filter_command, collapse = "\n"),
        "\n\nGROUP\n",
        paste(group_command, collapse = "\n"),
        "\n\nSUMMARISE\n",
        paste(names(summary_command),"=",summary_command, collapse = "\n"),
        "\n\nRENAME\n",
        paste(names(rename_command),"=",rename_command, collapse = "\n"),
        "\n\nMUTATE\n",
        paste(names(mutate_command),"=",mutate_command, collapse = "\n"),
        "\n\nSELECT\n",
        paste(select_command, collapse = "\n")
      )
      save_code_to_script(tmp, "summary.R", debug_folder)
    }
    # write out for debug - remote SQL table
    if(!is.na(debug_folder) && is_sql && exists("sql_query")){
      save_code_to_script(sql_query, "summary.sql", debug_folder)
    }
    
    ## filter out NAs if required ----
    
    if(status == "Successful completion" && remove_na_from_groups){
      group_cols = colnames(this_df)
      group_cols = group_cols[grepl("^group", group_cols, ignore.case = TRUE)]
      group_suffix = gsub("^group", "", group_cols)
      
      # only discard rows where grplabel is not NA but group is NA
      # so keep where grplabel is NA or group is not NA
      filter_commands = glue::glue("(is.na(grplabel{group_suffix}) | !is.na(group{group_suffix}))")
      
      this_df = dplyr::filter(this_df, !!!rlang::parse_exprs(filter_commands))
    }
    
    ## write to disk ----
    
    if(status == "Successful completion"){
      # ensure label and group columns are character
      text_cols = colnames(this_df)
      text_cols = text_cols[grepl("^(grplabel|group|label)", text_cols, ignore.case = TRUE)]
      for(cc in text_cols){
        this_df[[cc]] = as.character(this_df[[cc]])
      }
      
      # create directory
      out_file = loaded_cf$file[ii]
      if(!dir.exists(dirname(out_file))){
        dir.create(dirname(out_file), recursive = TRUE)
      }
      
      # write
      utils::write.table(
        this_df,
        out_file,
        append = file.exists(out_file),
        sep = ",",
        dec = ".",
        col.names = !file.exists(out_file),
        row.names = FALSE,
        qmethod = "double"
      )
    }

    ## conclude ----
    end_time = as.character(Sys.time())
    msg = sprintf("Summary step %3d of %d: %s", ii, nrow(loaded_cf), status)
    run_time_inform_user(msg)
    
    result_df$start_time[ii] = start_time
    result_df$end_time[ii] = end_time
    result_df$status[ii] = status
  }
  
  ## conclude ----
  
  run_time_inform_user("Summary tool complete.")
  return(invisible(result_df))
}
