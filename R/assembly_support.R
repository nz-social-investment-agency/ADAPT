################################################################################
#' Notes
#' 
################################################################################

## Alter table - drop column statement ------------------------------------ ----
#' Create alter table statement to drop columns
#' 
#' @param table_name The name of the table from which columns should be dropped.
#' It is recommended to use the full table name: database.schema.table
#' @param columns An array containing the name of the column or columns to drop.
#' @param if_exists T/F whether the query should account for existence of
#' columns. If TRUE, adds text "IF EXISTS" to result. If FALSE, executing the
#' query will error if any of the listed columns are not present.
#' 
#' @return A character string with an SQL Server query for dropping columns.
#' 
alter_table_drop_column = function(table_name, columns, if_exists = TRUE){
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(is.character(columns) & length(columns) >= 1)
  stopifnot(if_exists %in% c(TRUE, FALSE))
  
  table_name = add_delimiters(table_name, delimiter = "[]")
  
  if_exists = ifelse(if_exists, " IF EXISTS ", " ")
  
  columns = sapply(columns, add_delimiters, delimiter = "[]")
  columns = glue::glue("COLUMN{if_exists}{columns}")
  columns = paste0(columns, collapse = "\n , ")
  
  query = glue::glue("ALTER TABLE {table_name} DROP\n {columns}")
  
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
#' 
#' @return A character string with an SQL Server query for adding columns.
#' 
#' 
alter_table_add_column = function(table_name, columns, types){
  stopifnot(is.character(table_name) & length(table_name) == 1)
  stopifnot(is.character(columns) & length(columns) >= 1)
  stopifnot(is.character(types) & length(types) >= 1)
  stopifnot(length(columns) == length(types))
  
  table_name = add_delimiters(table_name, delimiter = "[]")
  
  types = toupper(types)
  stopifnot(all(sapply(types, is_valid_data_type)))
  
  columns = sapply(columns, add_delimiters, delimiter = "[]")
  columns = glue::glue("{columns} {types}")
  columns = paste0(columns, collapse = "\n , ")
  
  query = glue::glue("ALTER TABLE {table_name} ADD\n {columns}")
  
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
#' @param prefix The alias prefix for the source table in the query.
#' @param sqlite T/F is the connection SQLite (different handling is required
#' as SQLite does not have a DATE data type).
#'
#' @return Text containing the SQL calculation. When method is ENTITY, this
#' text will have length = 2.
#' 
handle_summary_case = function(control_file_row, prefix = "dmt", sqlite = FALSE){
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
  m_value = glue::glue("{prefix}.{control_file_row$measure_value}")
  o_name = control_file_row$output_name
  
  # data range supporting calculations
  p_start = glue::glue("{prefix}.{control_file_row$period_start}")
  p_end = glue::glue("{prefix}.{control_file_row$period_end}")
  m_start = glue::glue("{prefix}.{control_file_row$measure_start}")
  m_end = glue::glue("{prefix}.{control_file_row$measure_end}")
  
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
    value = glue::glue("IIF(COUNT({m_value}) >= 1, 1, 0) AS {o_name}")
    
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
      value = glue::glue("SUM(1 + {numerator}) AS {o_name}")

  } else {
    stop("unrecognised summary_type")
    
  }
  
  return(value)
}
