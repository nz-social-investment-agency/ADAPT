#' Execute confidentialisation tool.
#' 
#' @param control_file location of the control file containing
#' confidentialisation instructions to read into R. Accepted `.csv` and `.xlsx`
#' file formats.
#' @param sheet Sheet to read if control file is `.xlsx` format. As per
#' `openxlsx2::read_xlsx`: either a string (name of a sheet), or an integer
#' (the position of the sheet). Defaults to the first sheet otherwise.
#' @param tbl a data frame to confidentialise. Should be in local R memory.
#' @param stable_above Minimum source value for which consistent seeds are
#' required. Defaults to 30, because in practice smaller values come from
#' different sources.
#' 
#' @return A data frame created by applying to `tbl` the confidentialisation
#' rules defined by `control_file`.
#' 
#' @details
#' The best way to understand the confidentiality process is to review a worked
#' example. Try `provide_example` for worked examples, or for example control
#' files.
#' 
#' Control files are validated prior to execution. To validate a control file
#' without execution use `validate_confidential_control_file`.
#' 
#' All `control_file` values are optional. Where no value is provided, no action
#' is taken.
#' 
#' The first column of `control_file` should contain the command type as listed
#' below. The remaining columns should match the column names of `tbl`. The
#' accepted commands for the first column are:
#' * DROP - where this value is true, the column will be dropped from the
#' output. Main use case is to remove unneeded label columns.
#' * RENAME - text given in this row is used to rename columns. No checks are
#' applied to ensure the text makes sensible column names.
#' * MISSING_TO - numeric value to replace missing values in the column with.
#' Main use case is to set default values for entity count columns when entity
#' counts are not applicable.
#' * SEED - an optional command, if provided is used to set seeds for any
#' random rounding. Can be produced using the SEED type in the summary tool.
#' When a seed column is provided, the provided seeds are used rather than
#' generating stable seeds- this means that `stable_above` has no effect.
#' * ROUND - the type of rounding to apply. Accepted options are: RR3 and GRR
#' (for random rounding and graduated random rounding), and CONV10, CONV100, and
#' CONV1000 (for conventional rounding to base 10, 100, or 1000).
#' * SUPPRESS - when to apply suppression, an instruction of the form:
#' 'column-name < value'. Accepts input column names and renamed columns.
#' Multiple SUPPRESS rows are accepted, where there are multiple suppression
#' conditions add one per row.
#' * NOTES - this row is ignored and does not effect output. It is
#' intended for adding notes to the control file. You can have as many note rows
#' as you want.
#' 
#' Columns that have had rounding or suppression applied will appear twice in
#' the output: once with their original values and a second time with rounded / 
#' suppressed values. This second column is named with the prefix "conf_*".
#'  
#' Random rounding and Graduated random rounding via `run_confidential` include
#' handling of stable rounding and rounding consistent with thresholds.
#' * Stable rounding - means that each unique value in the same column greater
#' than `stable_above` is rounded with the same random seed. This is a more
#' conservative way of ensuring that counts of identical groups of people are
#' always rounded the same way.
#' * Rounding consistent with thresholds - ensures that where a column is used
#' for suppression and is also randomly rounded, then values at least/below a
#' suppression threshold remain at least/below the threshold after rounding.
#' This prevents the presence/absence of suppression being used to recover the
#' the raw unrounded value.
#' 
#' @md
#' 
#' @export
run_confidential = function(control_file, sheet = NULL, tbl, stable_above = 30){
  stopifnot(is.character(control_file), file.exists(control_file))
  stopifnot(is.null(sheet) | is.character(sheet))
  stopifnot(is.data.frame(tbl))
  
  ## load control file ----
  
  loaded_cf = load_control_file(control_file, sheet = sheet)
  
  ## initialize ----
  
  valid_control_file = validate_confidential_control_file(loaded_cf, tbl)
  stopifnot(valid_control_file)
  
  conf_cmds = tolower(loaded_cf[,1])
  ncols = ncol(loaded_cf)
  
  ## setup for generation ----
  
  # setup
  suppress_entries = loaded_cf[conf_cmds == "suppress",2:ncols]
  suppress_entries = suppress_entries[!is.na(suppress_entries)]
  suppress_entries = unique(suppress_entries)
  
  suppress_entries_df = lapply(suppress_entries, suppression_format_extract)
  suppress_entries_df = dplyr::bind_rows(c(
    suppress_entries_df,
    list(list(input = character(), column = character(), sign = character(), threshold = numeric(), valid = logical()))
  ))
  
  ## convert NAs ----
  
  treat_na_row = loaded_cf[conf_cmds == "missing_to",2:ncols, drop = FALSE]
  treat_na_row = treat_na_row[1,!is.na(treat_na_row), drop = FALSE]
  
  mutate_command = glue::glue("dplyr::coalesce({names(treat_na_row)}, {as.numeric(treat_na_row)})")
  names(mutate_command) = names(treat_na_row)
  
  tbl = dplyr::mutate(tbl, !!!rlang::parse_exprs(mutate_command))
  
  ## renaming columns ----
  
  rename_row = loaded_cf[conf_cmds == "rename", 2:ncols, drop = FALSE]
  rename_row = rename_row[1,!is.na(rename_row), drop = FALSE]
  from_name = colnames(rename_row)
  to_name = unlist(rename_row, use.names = FALSE)
  
  rename_command = from_name
  names(rename_command) = to_name
  
  # rename tbl
  tbl = dplyr::rename(tbl, !!!rlang::parse_exprs(rename_command))
  
  # rename suppression rules
  suppress_entries_df$column = sapply(
    suppress_entries_df$column,
    function(x){
      ifelse(x %not_in% from_name, x, to_name[from_name == x])
    })
  
  ## round and suppression ----
  
  round_and_suppress = loaded_cf[conf_cmds %in% c("round", "suppress"), 2:ncols, drop = FALSE]
  round_and_suppress = dplyr::rename(round_and_suppress, !!!rlang::parse_exprs(rename_command))
  
  source_seeds = loaded_cf[conf_cmds == "seed", 2:ncols, drop = FALSE]
  source_seeds = dplyr::rename(source_seeds, !!!rlang::parse_exprs(rename_command))
  
  # iterate through columns
  for(cc in colnames(round_and_suppress)){
    # skip if all NA = no rules
    if(all(is.na(round_and_suppress[[cc]]))){ next }
    
    # create column to confidentialise 
    new_col = glue::glue("conf_{cc}")
    tbl[[new_col]] = tbl[[cc]]
    
    # iterate through instructions
    for(instruction in round_and_suppress[[cc]]){
      # skip invalid instructions
      if(is.na(instruction)){ next }
      
      # seeds if RR3 or GRR
      if(instruction %in% c("RR3", "GRR")){
        
        # were seeds provided
        seed_col = NA
        seed_row_in_control_file = length(source_seeds[[cc]]) != 0
        if(seed_row_in_control_file){
          seed_col = source_seeds[[cc]][1]
        }
        
        # make seeds if not provided
        if(is.na(seed_col)){
          seeds = create_stable_seeds(tbl[[new_col]], stable_above = stable_above)
        } else {
          seeds = tbl[[seed_col]]
        }
        
      }
      
      # rounding
      if(instruction == "RR3"){
        threshold = unique(suppress_entries_df$threshold[suppress_entries_df$column == cc])
        tbl[[new_col]] = apply_random_rounding(tbl[[new_col]], seeds = seeds, threshold = threshold)
        next
      }
      if(instruction == "GRR"){
        threshold = unique(suppress_entries_df$threshold[suppress_entries_df$column == cc])
        tbl[[new_col]] = apply_graduated_random_rounding(tbl[[new_col]], seeds = seeds, threshold = threshold)
        next
      }
      if(instruction == "CONV10"){
        tbl[[new_col]] = apply_conventional_rounding(tbl[[new_col]], base = 10)
        next
      }
      if(instruction == "CONV100"){
        tbl[[new_col]] = apply_conventional_rounding(tbl[[new_col]], base = 100)
        next
      }
      if(instruction == "CONV1000"){
        tbl[[new_col]] = apply_conventional_rounding(tbl[[new_col]], base = 1000)
        next
      }
      
      # suppression
      if(instruction %in% suppress_entries_df$input){
        
        suppress_column = suppress_entries_df$column[suppress_entries_df$input == instruction]
        suppress_threshold = suppress_entries_df$threshold[suppress_entries_df$input == instruction]
        
        tbl[[new_col]] = apply_small_count_suppression(tbl[[new_col]], with = tbl[[suppress_column]], threshold = suppress_threshold)
        next
      }
      
      # else warn - this line should be unreachable
      msg = glue::glue("Unrecognised instruction: {instruction}")
      warning(msg)
      
    } # end iterate through instructions
  } # end iterate through columns
  
  ## drop unwanted columns ----
  
  drop_row = loaded_cf[conf_cmds == "drop",2:ncols, drop = FALSE]
  drop_row = drop_row[1,!is.na(drop_row), drop = FALSE]
  drop_row = drop_row[1,tolower(drop_row) %in% c("true", "1", "t", "yes", "drop"), drop = FALSE]
  
  tbl = dplyr::select(tbl, -dplyr::all_of(colnames(drop_row)))
  
  ## conclude ----
  return(tbl)
}
