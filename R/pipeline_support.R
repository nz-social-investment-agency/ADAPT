## Remove SQL comments ---------------------------------------------------- ----
#' Locate and remove in-line and multi-line SQL comments
#'
#' @param code A character string containing the SQL code.
#'
#' @return Code with all comments removed, but no changes to newline characters
#' so the line numbering is identical.
#' 
#' @details
#' SQL have two types of comments. in-line comments start with `--` and end at
#' the end of the line. Multi-line comments start with `/*` and end with `*/`.
#' 
#' The complexity of removing comments arises when these types interact. For
#' example, how should the line `-- */ text ` be understood - is the text within
#' a comment or not?
#' 
#' Testing in SQL Server shows that where comments interact, the comment that
#' starts first lasts until it ends and any comment-markers within it are
#' ignored. To handle this we need to scan through the code string rather than
#' using simple find and replace.
#' 
#' @export
remove_sql_comments = function(code){
  stopifnot(is.character(code))
  
  # setup
  df = data.frame(char_num = seq_len(nchar(code)))
  df$character = sapply(df$char_num, function(x){ substr(code, x, x) })
  df$char_pair = sapply(df$char_num, function(x){ substr(code, x, x + 1) })
  df = dplyr::mutate(
    df,
    is_newline = .data$character == "\n",
    state = NA_integer_
  )
  
  # iterate through setting states
  #  0 = not comment
  # -1 = in-line comment
  # 1+ = multi-line comment
  state = 0
  new_state = 0
  
  for(ii in df$char_num){
    
    if(df$char_pair[ii] == "--" && state == 0){
      new_state = -1
    }
    if(df$character[ii] == "\n" && state == -1){
      new_state = 0
    }
    if(df$char_pair[ii] == "/*" && state >= 0){
      new_state = state + 1
    }
    if(df$char_pair[ii] == "*/" && state > 0){
      new_state = state - 1
    }
    df$state[ii] = new_state
    state = new_state
  }
  
  # conclude
  df = dplyr::mutate(
    df,
    lag2_state = dplyr::lag(.data$state, 2, default = 0),
    is_comment = .data$state != 0 | .data$lag2_state > 0,
    out_char = ifelse(!.data$is_comment | .data$is_newline, .data$character, "")
  )
  
  return(paste0(df$out_char, collapse = ""))
}

## Standardise around SQL command mode ------------------------------------ ----
#' Replace SQL command mode variables with their values
#'
#' @description
#' This function searches SQL code to identify instances where SQL CMD Mode
#' variables have been set and replaces these with their values. The intention
#' is to enable the code to be called from R which does not enable CMD Mode.
#' Any `:SETVAR` statements are replaced with empty space, but new lines are
#' preserved.
#'
#' @param code_string A character vector comprising sql code
#'
#' @return A character vector comprising sql code with any command mode
#' `:SETVAR` statements removed, and variables replaced with values
#' 
#' @export
# Original by Dan Young, ValidateCodeR, used with permission.
sql_cmd_mode_fix = function(code_string){
  stopifnot(is.character(code_string))
  stopifnot(length(code_string) == 1)
  
  pattern =':setvar\\s+[[:alnum:]\\-\\_\\[\\]]+\\s+"?.*"?[\r\t\f\v]*(?=\\n)'
  found = regexpr(pattern, code_string, perl = TRUE, ignore.case = TRUE)
  
  limiter = 0
  
  # stop once :setvar not found
  while(found != -1){
    # found text
    found_text = substr(code_string, found, found + attr(found, "match.length") -1)
    triple = strsplit(found_text, "\\s")[[1]]
    
    # variable
    var = glue::glue("$({triple[2]})")
    # value
    val = triple[3]
    # substitution
    code_string = gsub(var, val, code_string, fixed = TRUE)
    
    # remove found text
    code_string = sub(found_text, "", code_string, fixed = TRUE)
    
    # re-search for a setvar
    found = regexpr(pattern, code_string, perl = TRUE, ignore.case = TRUE)
    
    # limiter
    limiter = limiter + 1
    if(limiter > 100){
      warning("sql_cmd_mode_fix stuck")
      break
    }
  }
  
  return(code_string)
}

## Read and prepare SQL code ---------------------------------------------- ----
#' Read in an SQL file and prepare for execution via ODBC
#' 
#' @param file_name_and_path the location of the SQL file to read.
#' 
#' @return A list with three components of equal length, where the length of
#' each component matches the number of batches in the code file. The components
#' are:
#' * `code` containing the code for the batch.
#' * `start_lines` containing the line number of the first line of the batch.
#' * `end_lines` containing the line number of the last line of the batch.
#' @md
#' 
#' @details
#' Preparation for ODBC includes: (1) removing comments, and (2) splitting code
#' into batches by breaking on `;` and by `GO`.
#' 
#' @export
read_and_prepare_sql_code = function(file_name_and_path){
  stopifnot(is.character(file_name_and_path))
  stopifnot(tools::file_ext(file_name_and_path) == "sql")
  stopifnot(file.exists(file_name_and_path))
  
  sql_code = readLines_utf8(file_name_and_path)
  sql_code = c(sql_code, "")
  sql_code = paste(sql_code, collapse = "\n")
  sql_code = remove_sql_comments(sql_code)
  sql_code = sql_cmd_mode_fix(sql_code)
  
  # triple all semi-colons string literals (within text strings)
  # for example ';' becomes ';;;'
  # allows break into batches to avoid breaking on ;;;
  # which will prevent batching in the middle of a text string
  sql_characters = strsplit(sql_code, "")[[1]]
  semicolon_string_literal = cumsum(sql_characters == "'") %% 2 == 1 & sql_characters == ';'
  sql_characters[semicolon_string_literal] = ';;;'
  sql_code = paste0(sql_characters, collapse = "")
  
  # break into batches
  # break on ; or a line containing only GO and whitespace
  # uses look-behind (?<=) and ahead (?=) to leave newlines in place
  # requires perl to use look-behind/ahead
  pattern = "(?<!;);(?!;)|(?<=\n)[\r\t\f\v ]*[gG][oO][\r\t\f\v ]*(?=\n)"
  sql_code = strsplit(sql_code, pattern, perl = TRUE)
  sql_code = unlist(sql_code, use.names = FALSE)
  
  # any triple semi-colons become single semi-colons again
  sql_code = gsub(';;;', ';', sql_code)
  
  # calculate start and end lines
  sql_code_short = gsub("\n", "", sql_code)
  batch_lines = nchar(sql_code) - nchar(sql_code_short)
  
  batch_starts = 1 + cumsum(c(0, batch_lines[-length(batch_lines)]))
  batch_ends = batch_starts + batch_lines
  
  stopifnot(length(sql_code) == length(batch_starts))
  stopifnot(length(sql_code) == length(batch_ends))
  
  return(list(code = sql_code, start_lines = batch_starts, end_lines = batch_ends))
}

## execute R code file ---------------------------------------------------- ----
#' Execute R script in separate environment
#'
#' @param file The file (and path) of the R script to execute. Errors if file
#' does not exist or is not a .R file.
#' @param injection A list containing named values. The separate environment
#' will be populated with these as variables before the file is executed (see
#' details).
#' @param ignore_warnings T/F whether execution should continue even if warnings
#' occur. If `FALSE` (the default) then will stop on the first warning and
#' return the warning message. If `TRUE` then will suppress all warnings.
#'
#' @return A list containing three components:
#' * status - Status message, of success or stopped with error/warning and the
#' error/warning message.
#' * start_time - the system time when the file started running.
#' * end_time - the system time when the file ceased running.
#' @md
#' 
#' @details
#' The R script is executed in a new environment. This means that variables
#' in the calling environment (e.g. within the pipeline tool) can not be used
#' or changed by the `file`, and that variables created by `file` do not persist
#' once its execution ends. Note that library calls within `file` do persist
#' once its execution ends due to how R handles packages. We do not recommend
#' using `library` within pipeline R scripts (use `package::function` instead).
#' 
#' If you want to run multiple R scripts that interact (for example a setup
#' script followed by a processing script), you can not run them all via this
#' function because each will be executed in an isolated environment.
#' 
#' The best alternative is to create an overview script that runs all the
#' interacting scripts, and use this function to run just this overview script.
#' 
#' `injection` provides a way to insert dynamic values into the environment. For
#' example `injection = list(ext = "csv")` would be equivalent to adding the
#' code `ext <- "csv"` at the top of the script being sourced. Case sensitive.
#' `injection` exists to allow parameters to be passed to scripts.
#' 
#' @export
try_run_R_file = function(file, injection = list(), ignore_warnings = FALSE){
  stopifnot(is.character(file), file.exists(file))
  stopifnot(tools::file_ext(file) %in% c("R", "r"))
  stopifnot(is.list(injection))
  stopifnot(length(injection) == length(unique(names(injection))))
  stopifnot(ignore_warnings %in% c(TRUE, FALSE))
  
  start_time = as.character(Sys.time())
  
  # execute, capturing messages
  status = tryCatch(
    {
      new_environment = new.env(parent = globalenv())
      # injection assignment into environment
      for(inj in names(injection)){
        new_environment[[inj]] = injection[[inj]]
      }
      
      if(ignore_warnings){
        suppressWarnings(source(file, local = new_environment))
      } else {
        source(file, local = new_environment)
      }
      "Successful completion"
    },
    error = function(e){
      msg = paste(c(e$message, e$body), collapse = "\n")
      msg = paste("Stopped with error: ", msg)
      return(msg)
    },
    warning = function(w){
      msg = paste(c(w$message, w$body), collapse = "\n")
      msg = paste("Stopped with warning: ", msg)
      return(msg)
    }
  )
  
  # conclude
  end_time = as.character(Sys.time())
  return(list(status = status, start_time = start_time, end_time = end_time))
}

## execute SQL code file -------------------------------------------------- ----
#' Execute SQL script with batch handling
#'
#' @param file The file (and path) of the SQL script to execute. Errors if file
#' does not exist or is not a .sql file.
#' @param db_connection_string A connection string for connecting to the
#' database.
#' @param injection A list containing named values. Where the names are found
#' in the SQL code, these will be replaced with their values (see details).
#' @param ignore_warnings T/F whether execution should continue even if warnings
#' occur. If `FALSE` (the default) then will stop on the first warning and
#' return the warning message. If `TRUE` then will suppress all warnings.
#'
#' @return A list containing three components:
#' * status - Status message, of success or stopped with error/warning and the
#' error/warning message.
#' * start_time - the system time when the file started running.
#' * end_time - the system time when the file ceased running.
#' @md
#' 
#' @details
#' The SQL script is executed in the database environment on its own connection.
#' This means that temporary tables and environmental variables only apply to
#' the current script.
#' 
#' If a script could be opened in a fresh SQL session and executed immediately
#' then it should run via this function. In out testing the approach we take
#' handles the overwhelming majority of scripts. But it may not handle all.
#' 
#' SQL CMD mode is not supported. Declared variables may fail depending on the
#' batching within the file.
#' 
#' As per `read_and_prepare_sql_code`, SQL scripts are split into batches based
#' on `;` and `GO`. To ensure temporary tables are persist across batches each
#' batch is executed with `SET NOCOUNT ON` and we use the `DBI::dbExecute`
#' setting `immediate = TRUE`.
#' 
#' `injection` provides a way to insert dynamic values into an SQL script. It is
#' the equivalent of SQL CMD mode for the pipeline tool (SQL CMD mode does not
#' work as expected via ODBC connection). Case sensitive. For example
#' `injection = list("$(tbl)" = "[my_table]")` would replace all instances of
#' `$(tbl)` with `[my_table]` in the code. `injection` exists to allow
#' parameters to be passed to scripts.
#' 
#' @export
try_run_SQL_file = function(file, db_connection_string, injection = list(), ignore_warnings = FALSE){
  stopifnot(is.character(file), file.exists(file))
  stopifnot(tolower(tools::file_ext(file)) == "sql")
  stopifnot(is.list(injection))
  stopifnot(length(injection) == length(unique(names(injection))))
  stopifnot(ignore_warnings %in% c(TRUE, FALSE))
  
  start_time = as.character(Sys.time())
  sql_batches = read_and_prepare_sql_code(file)
  
  db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)
  DBI::dbExecute(db_connection, "SET NOCOUNT ON;", immediate = TRUE)
  on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
  
  # execute, capturing messages
  status = tryCatch(
    {
      for(ii in seq_len(length(sql_batches$code))){
        lines = glue::glue("{sql_batches$start_lines[ii]}-{sql_batches$end_lines[ii]}")
        # modify for handling temp tables
        this_code = sql_batches$code[ii]
        
        # injection assignment into environment
        for(inj in names(injection)){
          this_code = gsub(inj, injection[[inj]], this_code, fixed = TRUE)
        }
        
        if(ignore_warnings){
          result = suppressWarnings(DBI::dbExecute(db_connection, this_code, immediate = TRUE))
        } else {
          DBI::dbExecute(db_connection, this_code, immediate = TRUE)
        }
      }
      "Successful completion"
    },
    error = function(e){
      msg = paste(c(e$message, e$body), collapse = "\n")
      msg = glue::glue("Stopped with error (see lines {lines}): ", msg)
      return(msg)
    },
    warning = function(w){
      msg = paste(c(w$message, w$body), collapse = "\n")
      msg = paste("Stopped with warning (see lines {lines}): ", msg)
      return(msg)
    }
  )
  
  # conclude
  end_time = as.character(Sys.time())
  return(list(status = status, start_time = start_time, end_time = end_time))
}
