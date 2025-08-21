## Generate combinations of columns for summary --------------------------- ----
#' Creates the cross-products of its inputs as a data frame. Designed for
#' receiving column names as input and creating combinations to group-by in
#' analysis.
#'
#' @param ... any number of arrays to take the cross-product between. Converted
#' to character. NA values in an arrays mean that the combination without this
#' array is also produced.
#' @param always an array of values that should always be included in the output
#' combination. Converted to character.
#' @param drop.dupes.within T/F, controls whether duplicated values within each
#' output set are discarded. For example: `(a,b,c,a,b)` becomes `(a,b,c)`.
#' Note: dplyr::group_by requires no duplicates so turning this off
#' may produce errors if the output is used for summarizing results.
#' Defaults to TRUE.
#' @param drop.dupes.across T/F, control whether duplicated sets of values
#' across the output are discarded. For example: `list(c(a,b,c), c(a,c,b))`
#' will only output `c(a,b,c)`.
#' Defaults to TRUE.
#'
#' @return A data frame produced by taking the cross-products of each input
#' group, appending the always inputs, and tidying by removing duplication
#' where applicable.
#'
#' @details
#' Note: because `...` is given as the first argument, all other arguments
#' need to be named. Otherwise their values will be included in the `...`
#' argument.
#'
#' `cross_product_column_names` provides equivalent output in list format for
#' backwards compatability.
#'
#' @examples
#' # basic use case
#' generate_combinations_df(10:11, 101:102, always = 1:3)
#'
#' # use of NAs for every combination
#' generate_combinations_df(c("a",NA),c("b",NA),c("c",NA))
#'
#' # differences when including duplicates
#' generate_combinations_df(1:3, 1:3)
#' generate_combinations_df(1:3, 1:3, drop.dupes.across = FALSE)
#' generate_combinations_df(1:3, 1:3, drop.dupes.within = FALSE)
#'
#' # applied example - every pairwise combination of demographic columns
#' demo_cols = c("age", "region", "sex", "ethnicity")
#' generate_combinations_df(demo_cols, demo_cols)
#'
#' @export
generate_combinations_df = function(
    ...,
    always = NULL,
    drop.dupes.within = TRUE,
    drop.dupes.across = TRUE
){
  stopifnot(is.logical(drop.dupes.within))
  stopifnot(is.logical(drop.dupes.across))
  # setup
  groups = list(...)
  groups = lapply(groups, as.character)
  output = list(as.character(always))
  
  # iterate through all combinations
  for(group in groups){
    new_list = list()
    for(grp_element in group){
      for(output_component in output){
        new_list = c(new_list, list(c(output_component, grp_element)))
      }
    }
    output = new_list
  }
  
  # drop duplicates in each line
  if(drop.dupes.within){
    output = lapply(
      output,
      function(x){
        x[duplicated(x)] = NA
        return(x)
      })
  }
  
  # remove duplicates across cross-products (ignores order)
  if(drop.dupes.across){
    sorted = sapply(output, function(v){ paste(sort(v), collapse = ",") })
    non_duplicates = !duplicated(sorted)
    output = output[non_duplicates]
  }
  
  # remove full NA
  all_NA = sapply(output, function(x){ all(is.na(x)) })
  output = output[!all_NA]
  
  # convert to data frame
  output = as.data.frame(t(as.data.frame(output)))
  rownames(output) = NULL
  colnames(output) = paste0("column", 1:ncol(output))
  
  return(output)
}

## Cross product ---------------------------------------------------------- ----
#' @rdname generate_combinations_df
#' @export
#' 
cross_product_column_names = function(
    ...,
    always = NULL,
    drop.dupes.within = TRUE,
    drop.dupes.across = TRUE
){
  
  output_df = generate_combinations_df(
    ...,
    always = always,
    drop.dupes.within = drop.dupes.within,
    drop.dupes.across = drop.dupes.across
  )
  
  output_list = as.list(as.data.frame(t(output_df)))
  names(output_list) = NULL
  
  output_list = lapply(output_list, function(x){ x[!is.na(x)] })
  
  return(output_list)
}

## Expand compact summary notation ---------------------------------------- ----
#' Expand compact summary notation
#' 
#' By requiring users to list all combinations in the summary control file, the
#' tool maximizes transparency: a user looking at the control file can see
#' exactly what will be run. The downside of this is that the control file can
#' become very long.
#' 
#' This function provides an alternative way to generate a summary control file.
#' It accepts compact notation and expands it to multiple rows.#' 
#' 
#' @param compact_control_file a data frame containing a control file that may
#' have compact notation (see details) for expansion.
#' @param column_names a character array containing column names of the table.
#' Used to expand `*_suffix` and `prefix_*` notation. Not required if such
#' notation is not used.
#' @inheritParams generate_combinations_df
#'
#' @return a data frame containing a control file with any conpact notation
#' expanded. So what was one row in the compact input is multiple output rows.
#' 
#' @details
#' Three types of compact notation are accepted:
#' 1. The or: `|`. Text in a group column separated by `|` is split to create
#' create new rows, one for each option. So `a|b` will lead to two output rows,
#' one grouped by `a` and the other by `b`.
#' 2. The no-group: `|+`. Text in a group column with `|+` on the end will also
#' create the upgrouped option. So `a|+` will lead to two output rows, one
#' grouped by `a` and the other without a value.
#' 3. Prefix and suffix pattern matching: `*_suffix` and `prefix_*`. Text in a
#' group column with either of these patterns will search for all options in
#' `column_names` that follow the same pattern. This is equivalent to listing
#' all the columns separated by `|` when the columns all share the same prefix
#' or suffix. Note that prefixes and suffixes must include an underscore
#' next to the `*`.
#' @md
#' 
#' @export
expand_compact_summary_groups = function(
    compact_control_file,
    column_names = character(0),
    drop.dupes.within = TRUE,
    drop.dupes.across = TRUE
){
  stopifnot(is.data.frame(compact_control_file))
  stopifnot(is.character(column_names))
  stopifnot(drop.dupes.within %in% c(TRUE, FALSE))
  stopifnot(drop.dupes.across %in% c(TRUE, FALSE))
  
  ## find group columns ----
  original_colnames = colnames(compact_control_file)
  colnames(compact_control_file) = trimws(tolower(colnames(compact_control_file)))
  
  group_cols = grep("^group", colnames(compact_control_file), value = TRUE)
  
  ## prefix,suffix, and unnest helper functions ----
  prefix_handler = function(x){
    if(!grepl("_\\*$", x)){ return(x) }
    pattern = substring(x, 1, nchar(x) - 1)
    prefix_matches = grep(pattern, column_names, value = TRUE, fixed = TRUE)
    stopifnot(length(prefix_matches) >= 1)
    return(prefix_matches)
  }
  vec_prefix_handler = function(x){ lapply(x, prefix_handler) }
  
  suffix_handler = function(x){
    if(!grepl("^\\*_", x)){ return(x) }
    pattern = substring(x, 2, nchar(x))
    suffix_matches = grep(pattern, column_names, value = TRUE, fixed = TRUE)
    stopifnot(length(suffix_matches) >= 1)
    return(suffix_matches)
  }
  vec_suffix_handler = function(x){ lapply(x, suffix_handler) }
  
  unnest_handler = function(df){
    for(col in group_cols){
      df = tidyr::unnest(df, cols = dplyr::all_of(col))
    }
    return(df)
  }
  
  ## expand ----
  expanded_control_file = compact_control_file
  
  # '|+' to 'NA' (only at end of cell) and split on | (incl. white space)
  expanded_control_file = expanded_control_file |>
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), ~ gsub("\\|\\s*\\+\\s*$", "|NA", .))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), ~ strsplit(., "\\s*\\|\\s*")))
  
  expanded_control_file = unnest_handler(expanded_control_file)

  # 'NA' to NA
  expanded_control_file = dplyr::mutate(
    expanded_control_file,
    dplyr::across(dplyr::all_of(group_cols), ~ ifelse(. == "NA", NA_character_, .))
  )
  
  # prefix & suffix
  expanded_control_file = dplyr::mutate(
    expanded_control_file,
    dplyr::across(dplyr::all_of(group_cols), vec_prefix_handler)
  )
  expanded_control_file = unnest_handler(expanded_control_file)
  
  expanded_control_file = dplyr::mutate(
    expanded_control_file,
    dplyr::across(dplyr::all_of(group_cols), vec_suffix_handler)
  )
  expanded_control_file = unnest_handler(expanded_control_file)
  
  # as data frame so can use matrix insertion
  expanded_control_file = as.data.frame(expanded_control_file)
  
  ## drop dupes within ----
  if(drop.dupes.within){
    for(ii in seq_len(nrow(expanded_control_file))){
      row = expanded_control_file[ii,group_cols]
      row = unlist(row, use.names = FALSE)
      dupes = duplicated(row)
      expanded_control_file[ii,group_cols] = ifelse(!dupes, row, rep(NA_character_, length(row)))
    }
  }
  
  ## drop dupes across ----
  if(drop.dupes.across){
    sorted_expanded_control_file = expanded_control_file
    
    for(ii in seq_len(nrow(expanded_control_file))){
      row = expanded_control_file[ii,group_cols]
      row = sort(unlist(row, use.names = FALSE), na.last = TRUE)
      sorted_expanded_control_file[ii,group_cols] = row
    }
    
    dupes = duplicated(sorted_expanded_control_file)
    expanded_control_file = dplyr::filter(expanded_control_file, !dupes)
  }
  
  ## remove rows with no values in any group column ----
  expanded_control_file = expanded_control_file |>
    dplyr::filter(!dplyr::if_all(dplyr::all_of(group_cols), is.na))
  
  ## conclude ----
  colnames(expanded_control_file) = original_colnames
  return(expanded_control_file)
}

## Generate summary commands ---------------------------------------------- ----
#' Convert control file input to summary commands.
#' 
#' These summary commands are text string that can be used within
#' rlang::parse_exprs within dplyr::mutate.
#' 
#' Processes columns of types: "distinct", "count", "sum", "entity", "stddev",
#' and "seed".
#' 
#' @param summary_row a data frame containing 1 row.
#' @param is_sql T/F whether summary occurs in SQL context or not. Counting
#' distinct values in SQL always ignores missing values. Hence for
#' consistency `n_distinct` uses argument `na.rm = TRUE` if not SQL and no
#' argument if SQL (as this argument is not handled during SQL translation).
#' 
#' @return a named character array suitable for parse_exprs
#' 
generate_summary_commands = function(summary_row, is_sql = FALSE){
  stopifnot(nrow(summary_row) == 1)
  
  col_type = tolower(gsub("[0-9\\.]", "", colnames(summary_row)))
  
  command_types = c("distinct", "count", "sum", "entity", "stddev", "seed")
  summary_cols = summary_row[,col_type %in% command_types, drop = FALSE]
  summary_cols = summary_cols[,!is.na(summary_cols), drop = FALSE]
  
  distinct_code = ifelse(
    is_sql,
    "dplyr::n_distinct({this_contents})",
    "dplyr::n_distinct({this_contents}, na.rm = TRUE)"
  )
  
  summary_command = sapply(
    1:ncol(summary_cols),
    function(ii){
      # extract current values
      this_name = colnames(summary_cols)[ii]
      this_contents = summary_cols[[this_name]]
      
      # remove delimiter if dynamic R included
      this_contents = remove_delimiters(this_contents, "{}")
      
      # produce required command
      this_command = switch(
        tolower(gsub("[0-9\\.]", "", this_name)),
        distinct = distinct_code,
        count = "sum(ifelse(!is.na({this_contents}), 1, 0), na.rm = TRUE)",
        sum = "sum({this_contents}, na.rm = TRUE)",
        entity = "ifelse({distinct_code} == 0, NA, {distinct_code})",
        stddev = "sd({this_contents}, na.rm = TRUE)",
        seed = "sum({this_contents} %% 100, na.rm = TRUE)"
      )
      
      # glue to insert values
      this_command = glue::glue(glue::glue(this_command))
      
      # assign name of column & output
      names(this_command) = tolower(colnames(summary_cols)[ii])
      return(this_command)
    }
  )
  
  return(summary_command)
}

## Union all over entities ------------------------------------------------ ----
#' Take union of tbl to allow for combining of min and max entities
#'
#' @param summary_row A single row of the control file.
#' @param tbl a data frame to summarise. Can be in-memory or remote accessed
#' with dbplyr.
#' 
#' @returns The tbl modified for handling `*__min` and `*__max` entities if
#' required.
#' 
#' @details
#' The 'entity' summary type takes distinct values. It also allows for distinct
#' values over two columns `*__min` and `*__max`. The only practical way to
#' implement this is to take a union all of the table stacking the `*__min` and
#' `*__max` columns into a single column.
#' 
#' 
entity_union_all_conversion = function(summary_row, tbl){
  stopifnot(is.data.frame(summary_row))
  stopifnot(nrow(summary_row) == 1)
  stopifnot(is.data.frame(tbl) | dplyr::is.tbl(tbl))
  
  ## determine if extra entity handling is required ----
  entity_columns = grepl("^entity", tolower(colnames(summary_row)))
  entity_columns = unlist(summary_row[1,entity_columns], use.names = FALSE)
  entity_columns = entity_columns[!is.na(entity_columns)]
  missing_entity_columns = setdiff(entity_columns, colnames(tbl))
  
  ## exit asap if no entity ----
  if(length(missing_entity_columns) == 0){
    return(tbl)
  }
  
  ## extract required cols ----
  
  # entity column names
  new_entity_cols = missing_entity_columns
  min_entity_cols = glue::glue("{missing_entity_columns}__min")
  max_entity_cols = glue::glue("{missing_entity_columns}__max")
  
  # grouping column names
  group_cols = grepl("^group", tolower(colnames(summary_row)))
  group_cols = unlist(summary_row[1,group_cols], use.names = FALSE)
  group_cols = group_cols[!is.na(group_cols)]
  
  ## check for conflicts ----
  
  summary_cols = c("distinct", "count", "sum", "entity", "stddev")
  summary_cols = paste0("^", summary_cols, collapse = "|")
  summary_cols = grepl(summary_cols, tolower(colnames(summary_row)))
  summary_cols = unlist(summary_row[1,summary_cols], use.names = FALSE)
  summary_cols = summary_cols[!is.na(summary_cols)]
  
  for(cc in group_cols){
    # pass if grouping column not found in any summary column
    if(!any(grepl(paste0("\\b", cc, "\\b"), summary_cols))){ next }
    
    msg = "Can not use column for both group and summary when processing 2-column entities"
    msg = glue::glue("{msg}\nPlease review: {cc}.")
    stop(msg)
  }
  
  ## make first table ----
  
  mutate_commands = ifelse(min_entity_cols %in% colnames(tbl), min_entity_cols, NA_character_)
  names(mutate_commands) = new_entity_cols
  
  first_tbl = dplyr::mutate(tbl, !!!rlang::parse_exprs(mutate_commands))
  first_tbl = dplyr::select(first_tbl, dplyr::all_of(c(colnames(tbl), new_entity_cols)))
  
  ## make second table ----
  
  mutate_commands = ifelse(max_entity_cols %in% colnames(tbl), max_entity_cols, NA_character_)
  names(mutate_commands) = new_entity_cols
  
  non_group_cols = setdiff(colnames(tbl), group_cols)
  mutate_commands2 = rep(NA_character_, length(non_group_cols))
  names(mutate_commands2) = non_group_cols
  
  second_tbl = dplyr::mutate(tbl, !!!rlang::parse_exprs(c(mutate_commands, mutate_commands2)))
  second_tbl = dplyr::select(second_tbl, dplyr::all_of(c(colnames(tbl), new_entity_cols)))
  
  ## return union all ----
  return(dplyr::union_all(first_tbl, second_tbl))
}

## Column names to lower case --------------------------------------------- ----
#' Column names in control file to lower case
#' 
#' @param summary_control_file a data frame containing summary instructions.
#' @param tbl_cols an array with the column names of the table. Where these
#' are found in the control file set them to lower case.
#' 
#' @return The control file with cells contents set to lower case to match
#' lower case names of `tbl_cols`. Affects columns of type group, distinct,
#' count, sum, entity, stddev, and where.
#'  
#'  This internal function exists because R is case sentitive, but SQL is not,
#'  and control files might not be case sentivie either.
#'  
tolower_control_file_cells = function(summary_control_file, tbl_cols){
  stopifnot(is.data.frame(summary_control_file))
  stopifnot(is.character(tbl_cols))
  
  ## setup ----
  tbl_cols = tolower(trimws(tbl_cols))
  
  col_types_to_process = c("group", "distinct", "count", "sum", "entity", "stddev", "where")
  col_types_to_process = paste0("^", col_types_to_process, collapse = "|") # regex pattern
  relevant_cols = colnames(summary_control_file)
  relevant_cols = relevant_cols[grepl(col_types_to_process, relevant_cols, ignore.case = TRUE)]
  
  ## all contents ----
  all_contents = unlist(summary_control_file, use.names = FALSE)
  all_contents = unique(all_contents)
  all_contents = all_contents[!is.na(all_contents)]
  
  ## process each column ----
  for(col in relevant_cols){
    
    this_col = summary_control_file[[col]]
    
    # all non-dynamic values
    is_na = is.na(this_col)
    is_delim = is_delimited(this_col, "{}")
    this_col[!is_na & !is_delim] = tolower(trimws(this_col[!is_na & !is_delim]))
    
    # dynamic values
    unique_dynamics = unique(this_col[is_delim])
    improved_dynamics = unique_dynamics
    for(tt in tbl_cols){
      pattern = glue::glue("\\b{tt}\\b")
      improved_dynamics = gsub(pattern, tt, improved_dynamics, ignore.case = TRUE)
    }
    for(ii in seq_along(unique_dynamics)){
      this_col[this_col == unique_dynamics[ii]] = improved_dynamics[ii]
    }
    
    summary_control_file[[col]] = this_col
  }
  
  ## conclude ----
  return(summary_control_file)
}
