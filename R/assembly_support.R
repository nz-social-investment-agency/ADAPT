## Alter table - drop column statement ------------------------------------ ----
#' Create alter table statement to drop columns
#' 
#' @param table_name The name of the table from which columns should be dropped.
#' It is recommended to use the full table name: database.schema.table
#' @param columns An array containing the name of the column or columns to drop.
#' @param if_exists T/F whether the query should account for existence of
#' columns. If TRUE, adds text "IF EXISTS" to result. If FALSE, executing the
#' query will error if any of the listed columns are not present.
#' @param sqlite T/F is the connection SQLite (different handling is required
#' as SQLite does not have a DATE data type).
#'  
#' @return A character string with an SQL Server query for dropping columns.
#' 
#' @export
alter_table_drop_column = function(table_name, columns, sqlite = FALSE, if_exists = !sqlite){
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(if_exists %in% c(TRUE, FALSE))
  if(length(columns) == 0){ return(character(0)) }
  stopifnot(is.character(columns) & length(columns) >= 1)
  stopifnot(sqlite %in% c(TRUE, FALSE))
  
  table_name = add_delimiters(table_name, delimiter = "[]")
  
  if_exists = ifelse(if_exists, " IF EXISTS ", " ")
  
  columns = sapply(columns, add_delimiters, delimiter = "[]")
  columns = glue::glue("COLUMN{if_exists}{columns}")
  
  if(!sqlite){
    columns = paste0(columns, collapse = "\n    , ")
  }
  
  query = glue::glue("ALTER TABLE {table_name} DROP {columns}")
  
  return(as.character(query))
}

## Alter table - add column statement ------------------------------------- ----
#' Create alter table statement to add columns
#' 
#' @param table_name The name of the table to which columns should be added.
#' It is recommended to use the full table name: database.schema.table
#' @param columns An array containing the name of the column or columns to add.
#' @param types An array of the SQL data types that correspond to columns.
#' Will error if invalid SQL data types are provided.
#' @param sqlite T/F is the connection SQLite (different handling is required
#' as SQLite does not have a DATE data type).
#'  
#' @return A character string with an SQL Server query for adding columns.
#' 
#' @export
alter_table_add_column = function(table_name, columns, types, sqlite = FALSE){
  stopifnot(is.character(table_name) & length(table_name) == 1)
  if(length(columns) == 0){ return(character(0)) }
  stopifnot(is.character(columns) & length(columns) >= 1)
  stopifnot(is.character(types) & length(types) >= 1)
  stopifnot(length(columns) == length(types))
  stopifnot(sqlite %in% c(TRUE, FALSE))
  
  table_name = add_delimiters(table_name, delimiter = "[]")
  
  types = toupper(types)
  stopifnot(all(sapply(types, is_valid_data_type)))
  
  columns = sapply(columns, add_delimiters, delimiter = "[]")
  columns = glue::glue("{columns} {types}")
  
  if(!sqlite){
    columns = paste0(columns, collapse = "\n    , ")
  }
  sqlite = ifelse(sqlite, " COLUMN", "")
  
  query = glue::glue("ALTER TABLE {table_name} ADD{sqlite} {columns}")
  
  return(as.character(query))
}

## Accepted SQL data types ------------------------------------------------ ----
#' Check whether a data type is a valid SQL Server data type
#' 
#' @param type The data type to test.
#' 
#' @return T/F whether the type is an accepted SQL Server data type.
#' 
#' @details
#' The list of accepted types is as follows:
#' tinyint, smallint, int, bigint, bit, decimal, numeric, money, smallmoney,
#' float, real, date, time, datetime2, datetimeoffset, datetime, smalldatetime,
#' char, varchar, text, nchar, nvarchar, ntext.
#' 
#' Does not check numeric components of the type. For example, for input
#' `type = VARCHAR(5)`, only confirms the `VARCHAR` component.
#' 
is_valid_data_type = function(type){
  stopifnot(is.character(type))
  
  accepted_types = c("tinyint", "smallint", "int", "bigint", "bit", "decimal", 
                     "numeric", "money", "smallmoney", "float", "real", "date", 
                     "time", "datetime2", "datetimeoffset", "datetime", 
                     "smalldatetime", "char", "varchar", "text", "nchar", 
                     "nvarchar", "ntext")
  accepted_types = paste0("\\b", toupper(accepted_types), "\\b", collapse = "|")
  
  return(grepl(accepted_types, toupper(type)))
}

## File exists and contains ----------------------------------------------- ----
#' Check whether an SQL file exists and contains the specified text.
#' 
#' Used to estimate whether an SQL code file will create a table named `text`
#' or containing a column `text`.
#' 
#' @param file Path to the file to check for existence and contents.
#' @param text Text that needs to found within the file. Special characters
#' are taken as literal rather than as regular expression. Characters
#' `[` and `]` are made optional.
#' 
#' @return T/F Whether the file exists and contains the specified text.
#'  
sql_file_exists_and_contains = function(file, text){
  stopifnot(is.character(file))
  stopifnot(length(file) == 1)
  stopifnot(is.character(text))
  
  # FALSE if file does not exist
  if(!file.exists(file)){
    return(FALSE)
  }
  
  # FALSE if not SQL file
  if(tolower(tools::file_ext(file)) != "sql"){
    return(FALSE)
  }
  
  # read contents
  contents = readLines(file, warn = FALSE)
  contents = paste0(contents, collapse = "\n")
  
  # remove comments
  single_line_comment_pattern = "--.*?\\n"
  multi_line_comment_pattern = "(?s)/\\*.*?\\*/"
  
  contents = gsub(single_line_comment_pattern, "", contents)
  contents = gsub(multi_line_comment_pattern, "", contents, perl = TRUE)
  
  # pattern setup, [] are optional, word boundaries required
  special_characters = c(".", "+", "?", "^", "$", "(", ")", "{", "}", "|", "\\")
  for(char in special_characters){
    text = gsub(paste0("\\", char), paste0("\\\\", char), text)
  
  }
  text = gsub("\\[", "\\\\\\[?", text)
  text = gsub("\\]", "\\\\\\]?", text)
  
  # return text in file
  return(sapply(text, grepl, x = contents, USE.NAMES = FALSE))
}

## Entity control file row to two rows ------------------------------------ ----
#' Convert entity rows in control file to min and max rows
#' 
#' @param control_file a data frame containing assembly instructions. Most
#' likely read into memory by `load_control_file`.
#' 
#' @return The control file with ENTITY types removed and replaced by pairs of
#' MIN and MAX types.
#' 
#' @details
#' For the ENTITY summary method the assembly tool outputs two variables, with
#' `*__min` and `*__max` suffixes. Initial designs handled this as its own
#' summary method However, it is a simpler implementation to modify the control
#' file - replacing the ENTITY row with two rows - a MIN and a MAX. This means
#' the rest of the process never has to handle one row becoming two.
#' 
entity_to_min_and_max = function(control_file){
  stopifnot(is.data.frame(control_file))
  stopifnot("output_method" %in% colnames(control_file))
  stopifnot("output_name" %in% colnames(control_file))
  
  # rows with ENTITY
  ent_rows = which(control_file$output_method == "ENTITY")
  # reverse order else initial changes effect row numbers of later changes
  ent_rows = sort(ent_rows, decreasing = TRUE)
  
  # for each row
  for(rr in ent_rows){
    the_row = dplyr::slice(control_file, rr)
    
    min_row = the_row
    min_row$output_method = "MIN"
    min_row$output_name = gsub("(\"?)$", "__min\\1", min_row$output_name)
    
    max_row = the_row
    max_row$output_method = "MAX"
    max_row$output_name = gsub("(\"?)$", "__max\\1", max_row$output_name)
    
    row_numbers = 1:nrow(control_file)
    earlier_rows = row_numbers[row_numbers < rr]
    later_rows = row_numbers[row_numbers > rr]
    
    control_file = dplyr::bind_rows(
      dplyr::slice(control_file, earlier_rows),
      min_row,
      max_row,
      dplyr::slice(control_file, later_rows),
    )
  }
  
  return(control_file)
}

## Handle cases of assembly summary --------------------------------------- ----
#' Handle summary cases for assembly
#' 
#' A range of summary methods can be chosen for the output:
#' MIN, MAX, EXISTS, COUNT, MEAN, DISTINCT, ENTITY, SUM, SUM_WITHIN, DURATION.
#' 
#' Each of these needs to be translated into an SQL calculation for the form:
#' `method(input_column) AS output_column`.
#' These are used to build the SQL code required for assembly.
#' 
#' @param control_file_row A row on an assembly control_file.
#' @param sqlite T/F is the connection SQLite (different handling is required
#' as SQLite does not have a DATE data type).
#'
#' @return Text containing the SQL calculation. When method is ENTITY, this
#' text will have length = 2.
#' 
handle_summary_case = function(control_file_row, sqlite = FALSE){
  stopifnot(is.data.frame(control_file_row))
  stopifnot(nrow(control_file_row) == 1)
  stopifnot(sqlite %in% c(TRUE, FALSE))
  
  req_cols = c("period_start", "period_end", "measure_start", "measure_end", "measure_value", "output_name", "output_method")
  stopifnot(all(req_cols %in% colnames(control_file_row)))
  
  accepted_methods = c("MIN", "MAX", "EXISTS", "COUNT", "MEAN", "DISTINCT", "ENTITY", "SUM", "SUM_WITHIN", "DURATION")
  control_file_row$output_method = toupper(control_file_row$output_method)
  stopifnot(all(control_file_row$output_method %in% accepted_methods))
  
  # aliases for ease of reading
  method = control_file_row$output_method
  m_value = control_file_row$measure_value
  o_name = control_file_row$output_name
  
  # data range supporting calculations
  p_start = "dmt.core_query_p_start" # control_file_row$period_start
  p_end = "dmt.core_query_p_end" # control_file_row$period_end
  m_start = control_file_row$measure_start
  m_end = control_file_row$measure_end
  
  # later start date - earlier end date
  numerator = ifelse(
    sqlite,
    glue::glue(
      "JULIANDAY(IIF({m_end} < {p_end}, {m_end}, {p_end}))",
      " - ",
      "JULIANDAY(IIF({m_start} < {p_start}, {p_start}, {m_start}))"
    ),
    glue::glue(
      "DATEDIFF(DAY,",
      "IIF({m_start} < {p_start}, {p_start}, {m_start}),",
      "IIF({m_end} < {p_end}, {m_end}, {p_end}))"
    )
  )
  denominator = ifelse(
    sqlite,
    glue::glue("JULIANDAY({m_end}) - JULIANDAY({m_start})"),
    glue::glue("DATEDIFF(DAY, {m_start}, {m_end})")
  )
  
  # handle cases
  if (method == "MIN") {
    value = glue::glue("MIN({m_value}) AS {o_name}")
    
  } else if (method == "MAX") {
    value = glue::glue("MAX({m_value}) AS {o_name}")
    
  } else if (method == "EXISTS") {
    value = glue::glue("IIF(COUNT({m_value}) >= 1, 1, NULL) AS {o_name}")
    
  } else if (method == "COUNT") {
    value = glue::glue("COUNT({m_value}) AS {o_name}")
    
  } else if (method == "MEAN") {
    value = glue::glue("AVG({m_value}) AS {o_name}")
  
  } else if (method == "DISTINCT") {
    value = glue::glue("COUNT(DISTINCT {m_value}) AS {o_name}")
    
  } else if (method == "ENTITY") {
    value = c(
      glue::glue("MIN({m_value}) AS {o_name}__min"),
      glue::glue("MAX({m_value}) AS {o_name}__max")
    )
  
  } else if (method == "SUM") {
    value = glue::glue("SUM({m_value}) AS {o_name}")
    
  } else if (method == "SUM_WITHIN") {
    value = glue::glue("SUM(1.0 * (1 + {numerator}) / (1 + {denominator}) * {m_value}) AS {o_name}")

  } else if (method == "DURATION") {
      value = glue::glue("SUM(IIF({m_value} IS NULL, NULL, 1 + {numerator})) AS {o_name}")

  } else {
    stop("unrecognised summary_type")
    
  }
  
  return(value)
}

## Handle delimiters and SQL prefixes ------------------------------------- ----
#' Handle delimiters and SQL prefixes when preparing core assembly query
#' 
#' @param df A data frame extracted from an assembly control file.
#' @param mt_prefix The prefix to add to columns found in `mt_cols`.
#' @param mt_cols List of columns that should receive `mt_prefix`.
#' @param measure_prefix The prefix to add to columns found in `measure_cols`.
#' @param measure_cols List of columns that should receive `measure_prefix`.
#' 
#' @returns The input data frame prepared for using in making SQL queries.
#' 
#' @details
#' Modifies columns with the names: period_start, period_end, measure_start,
#' measure_end, and measure_value accoridng to the following rules:
#' * Inputs delimited with `"` becomes delimited by `'`. This changes constants
#' in the control file to text strings for SQL.
#' * Inputs delimited with `{}` have their delimiters removed. This changes
#' dynamic input in the control file to pure SQL.
#' * Columns in `mt_cols` are given the prefix `mt_prefix`. This makes the
#' source from which columns are fetched in the core query clear.
#' * Columns in `measure_cols` are given the prefix `measure_prefix`. This makes
#' the source from which columns are fetched in the core query clear.
#' 
#' The first pair of tasks are necessary because we are moving between
#' environments with different delimiters. The second pair of tasks are
#' necessary to avoid confusion when input tables have the same column name.
#' Regex approach required to handle cases where control_file contents are
#' constant or dynamic.
#' @md
#' 
handle_delimiters_and_prefixes = function(df, mt_prefix, mt_cols, measure_prefix, measure_cols){
  stopifnot(is.data.frame(df))
  stopifnot(is.character(mt_prefix) && length(mt_prefix) == 1)
  stopifnot(is.character(mt_cols))
  stopifnot(is.character(measure_prefix) && length(measure_prefix) == 1)
  stopifnot(is.character(measure_cols))
  
  # remove "" delimiters from output_name
  if("output_name" %in% colnames(df)){
    df$output_name = remove_delimiters(df$output_name, "\"")
  }
  
  cols_to_convert = c("period_start", "period_end", "measure_start", "measure_end", "measure_value")
  for(cc in base::intersect(cols_to_convert, colnames(df))){
    contents = df[[cc]]
    # delimiter " >> '
    to_single_quote = is_delimited(contents, "\"")
    contents[to_single_quote] = remove_delimiters(contents[to_single_quote], "\"")
    contents[to_single_quote] = add_delimiters(contents[to_single_quote], "'")
    # remove delimiter {}
    to_unquoted = is_delimited(contents, "{}")
    contents[to_unquoted] = trimws(remove_delimiters(contents[to_unquoted], "{}"))
    # done
    df[[cc]] = contents
  }
  
  # prefix for master table
  for(cc in base::intersect(c("period_start", "period_end"), colnames(df))){
    # prep regex - with []
    pattern = paste(mt_cols, collapse = "|")
    pattern = paste0("(\\[(", pattern, ")\\])")
    replacement = paste0(mt_prefix, ".\\1")
    # apply regex
    df[[cc]] = gsub(pattern, replacement, df[[cc]])
    # prep regex - with ""
    pattern = paste(mt_cols, collapse = "|")
    pattern = paste0("(\"(", pattern, ")\")")
    replacement = paste0(mt_prefix, ".\\1")
    # apply regex
    df[[cc]] = gsub(pattern, replacement, df[[cc]])
    # prep regex - without [] or ""
    pattern = paste(mt_cols, collapse = "|")
    pattern = paste0("(?<![[\"])\\b(", pattern, ")\\b(?![\"\\]])")
    replacement = paste0(mt_prefix, ".\\1")
    # apply regex
    df[[cc]] = gsub(pattern, replacement, df[[cc]], perl = TRUE)
  }

  # prefix for measure table
  for(cc in base::intersect(c("measure_start", "measure_end", "measure_value"), colnames(df))){
    # prep regex - with []
    pattern = paste(measure_cols, collapse = "|")
    pattern = paste0("(\\[(", pattern, ")\\])")
    replacement = paste0(measure_prefix, ".\\1")
    # apply regex
    df[[cc]] = gsub(pattern, replacement, df[[cc]])
    # prep regex - with ""
    pattern = paste(measure_cols, collapse = "|")
    pattern = paste0("(\"(", pattern, ")\")")
    replacement = paste0(measure_prefix, ".\\1")
    # apply regex
    df[[cc]] = gsub(pattern, replacement, df[[cc]])
    # prep regex - without [] or ""
    pattern = paste(measure_cols, collapse = "|")
    pattern = paste0("(?<![[\"])\\b(", pattern, ")\\b(?![\"\\]])")
    replacement = paste0(measure_prefix, ".\\1")
    # apply regex
    df[[cc]] = gsub(pattern, replacement, df[[cc]], perl = TRUE)
  }
  
  return(df)
}
