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

## Generate summary commands ---------------------------------------------- ----
#' Convert control file input to text string that can be used within
#' dplyr::mutate via rlang::parse_exprs.
#' 
#' Processes columns of types: "distinct", "count", "sum", "entity", "stddev"
#' 
#' @param summary_row a data frame containing 1 row.
#' 
#' @return a named character array suitable for parse_exprs
#' 
generate_summary_commands = function(summary_row){
  stopifnot(nrow(summary_row) == 1)
  
  col_type = tolower(gsub("[0-9\\.]", "", colnames(summary_row)))
  
  command_types = c("distinct", "count", "sum", "entity", "stddev")
  summary_cols = summary_row[,col_type %in% command_types, drop = FALSE]
  summary_cols = summary_cols[,!is.na(summary_cols), drop = FALSE]
  
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
        distinct = "dplyr::n_distinct({this_contents}, na.rm = TRUE)",
        count = "sum(ifelse(!is.na({this_contents}), 1, 0), na.rm = TRUE)",
        sum = "sum({this_contents}, na.rm = TRUE)",
        entity = "dplyr::n_distinct({this_contents}, na.rm = TRUE)",
        stddev = "sd({this_contents}, na.rm = TRUE)"
      )
      
      # glue to insert values
      this_command = glue::glue(this_command)
      
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
  stopifnot(is.data.frame(tbl))
  
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
