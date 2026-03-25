# database connection - requires SQL Server in environment
db_connection_string = "NA"

can_connect = DBI::dbCanConnect(odbc::odbc(), .connection_string = db_connection_string)

if(nchar(db_connection_string) > 5 & !can_connect){
  stop("SQL Server connection string should not be part of package")
}

## remove_sql_comments(code) ---------------------------------------------- ----

test_that("no comments unchanged", {
  code = "a\nb\nc d"
  actual = remove_sql_comments(code)
  expect_equal(actual, code)
})

test_that("in-line comments removed", {
  code = "a\n -- b \nc d"
  actual = remove_sql_comments(code)
  expected = "a\n \nc d"
  expect_equal(actual, expected)
})

test_that("multi-line comments removed", {
  code = "a\nb\nc /*d */e/*f\ng*/ "
  actual = remove_sql_comments(code)
  expected = "a\nb\nc e\n "
  expect_equal(actual, expected)
})

test_that("nested multi-line comments removed", {
  code = "\na/*\nb\n/*\nc\n*/\nd\n*/\ne\n/*\nf\n*/g"
  actual = remove_sql_comments(code)
  expected = "\na\n\n\n\n\n\n\ne\n\n\ng"
  expect_equal(actual, expected)
})

test_that("unclosed comments removed", {
  code = "a\n--b"
  actual = remove_sql_comments(code)
  expected = "a\n"
  expect_equal(actual, expected)
  
  code = "a/*b"
  actual = remove_sql_comments(code)
  expected = "a"
  expect_equal(actual, expected)
  
  code = "a/*b/*c*/d"
  actual = remove_sql_comments(code)
  expected = "a"
  expect_equal(actual, expected)
})

test_that("interacting comments removed", {
  
  code = "a\n-- /* b \n c \n */ d"
  actual = remove_sql_comments(code)
  expected = "a\n\n c \n */ d"
  expect_equal(actual, expected)
  
  code = "a /* b -- c */ d"
  actual = remove_sql_comments(code)
  expected = "a  d"
  expect_equal(actual, expected)
  
  code = "a /* b /* c */ d -- e */ \n f"
  actual = remove_sql_comments(code)
  expected = "a  \n f"
  expect_equal(actual, expected)
  
  code = "a -- b /* c \n d \n e -- f */ g"
  actual = remove_sql_comments(code)
  expected = "a \n d \n e "
  expect_equal(actual, expected)
})

## sql_cmd_mode_fix(code_string) ------------------------------------------ ----

test_that("no cmd mode unchanged", {
  input = paste0(
    c(
      "-- header",
      "",
      "SELECT *",
      "FROM mytable",
      "",
      "SELECT 3",
      ""
    ),
    collapse = "\n"
  )
  
  expect_equal(sql_cmd_mode_fix(input), input)
})

test_that("cmd mode cleans preserved line count", {
  input = paste0(
    c(
      "-- header",
      ":setvar a 1",
      " :SETVAR b \"2\"",
      "",
      "SELECT *",
      "FROM mytable",
      ":Setvar c 3\t",
      "",
      "SELECT 3",
      ""
    ),
    collapse = "\n"
  )
  
  output = paste0(
    c(
      "-- header",
      "",
      " ",
      "",
      "SELECT *",
      "FROM mytable",
      "",
      "",
      "SELECT 3",
      ""
    ),
    collapse = "\n"
  )
  
  expect_equal(sql_cmd_mode_fix(input), output)
})

test_that("substitutions occur", {
  input = paste0(
    c(
      "-- header",
      ":setvar a 1",
      " :SETVAR b \"2\"",
      "",
      "SELECT $(b)",
      "FROM mytable",
      ":Setvar c 3\t",
      "",
      "SELECT $(c) + $(c) - $(a)",
      ""
    ),
    collapse = "\n"
  )
  
  output = paste0(
    c(
      "-- header",
      "",
      " ",
      "",
      "SELECT \"2\"",
      "FROM mytable",
      "",
      "",
      "SELECT 3 + 3 - 1",
      ""
    ),
    collapse = "\n"
  )
  
  expect_equal(sql_cmd_mode_fix(input), output)
})

## read_and_prepare_sql_code(file_name_and_path) -------------------------- ----

test_that("simple case works", {
  tmp_dir = tempdir()
  sql_file = file.path(tmp_dir, "code.sql")
  
  code = "query1\nGO\nquery2\nquery3;\nquery4"
  writeLines(code, sql_file)
  
  actual = read_and_prepare_sql_code(sql_file)
  
  expected = list(
    code = c("query1\n","\nquery2\nquery3","\nquery4\n"),
    start_lines = c(1,2,4),
    end_lines = c(2,4,6)
  )

  expect_equal(names(actual), names(expected))
  for(nn in names(actual)){
    expect_equal(actual[[nn]], expected[[nn]])
  }
})

test_that("case with comments works", {
  tmp_dir = tempdir()
  sql_file = file.path(tmp_dir, "code.sql")
  
  code = "query1\n--query2\nGO\nquery3\n--GO\nquery4;\nGO\nquery5\n/*\nquery6\nGO\n*/\nquery7"
  writeLines(code, sql_file)
  
  actual = read_and_prepare_sql_code(sql_file)
  
  expected = list(
    code = c("query1\n\n","\nquery3\n\nquery4","\n","\nquery5\n\n\n\n\nquery7\n"),
    start_lines = c(1,3,6,7),
    end_lines = c(3,6,7,14)
  )
  
  expect_equal(names(actual), names(expected))
  for(nn in names(actual)){
    expect_equal(actual[[nn]], expected[[nn]])
  }
})

test_that("multiple newlines work", {
  tmp_dir = tempdir()
  sql_file = file.path(tmp_dir, "code.sql")
  
  code = "query1\n\n\nGO\n\n\nquery2\n"
  writeLines(code, sql_file)
  
  actual = read_and_prepare_sql_code(sql_file)
  
  expected = list(
    code = c("query1\n\n\n", "\n\n\nquery2\n\n"),
    start_lines = c(1,4),
    end_lines = c(4,9)
  )
  
  expect_equal(names(actual), names(expected))
  for(nn in names(actual)){
    expect_equal(actual[[nn]], expected[[nn]])
  }
})

test_that("comments and cmd mode works", {
  tmp_dir = tempdir()
  sql_file = file.path(tmp_dir, "code.sql")
  
  code = "query1/*comment*/\nGO\n:setvar var \"val\"\n\nquery2\n $(var) \n"
  writeLines(code, sql_file)
  
  actual = read_and_prepare_sql_code(sql_file)
  
  expected = list(
    code = c("query1\n","\n\n\nquery2\n \"val\" \n\n"),
    start_lines = c(1,2),
    end_lines = c(2,8)
  )
  
  expect_equal(names(actual), names(expected))
  for(nn in names(actual)){
    expect_equal(actual[[nn]], expected[[nn]])
  }
})

test_that("semi-colon in text string does not break", {
  tmp_dir = tempdir()
  sql_file = file.path(tmp_dir, "code.sql")
  
  code = "query1 = ';'\nquery2 = ';;'"
  writeLines(code, sql_file)
  
  actual = read_and_prepare_sql_code(sql_file)
  
  expect_true(length(actual$code) == 1)
})

## try_run_R_file(file, ignore_warnings) ---------------------------------- ----

test_that("R files run", {
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "fizzbuzz.R")
  
  actual = try_run_R_file(test_file)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_equal(actual$status, "Successful completion")
})

test_that("warnings in R handled", {
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "fizzbuzz_w_warning.R")
  
  actual = try_run_R_file(test_file, ignore_warnings = FALSE)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_true(grepl("warning", actual$status))
  expect_true(grepl("test R warning", actual$status))
  
  actual = try_run_R_file(test_file, ignore_warnings = TRUE)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_equal(actual$status, "Successful completion")
})

test_that("errors in R handled", {
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "fizzbuzz_w_error.R")
  
  actual = try_run_R_file(test_file, ignore_warnings = FALSE)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_true(grepl("error", actual$status))
  expect_true(grepl("test R error", actual$status))
  
  actual = try_run_R_file(test_file, ignore_warnings = TRUE)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_true(grepl("error", actual$status))
  expect_true(grepl("test R error", actual$status))
})

test_that("R injection performs", {
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "errors_wout_injection.R")
  
  # without injection error
  actual = try_run_R_file(test_file)
  expect_true(grepl("error", actual$status))
  
  # with injection success
  actual = try_run_R_file(test_file, injection = list(my_value = 10))
  expect_equal(actual$status, "Successful completion")
})

## try_run_SQL_file(file, db_connection_string, ignore_warnings) ---------- ----

test_that("SQL files run", {
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "setup.sql")
  
  actual = try_run_SQL_file(test_file, db_connection_string, ignore_warnings = FALSE)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_equal(actual$status, "Successful completion")
})

test_that("errors in SQL handled", {
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "calculate.sql")
  
  actual = try_run_SQL_file(test_file, db_connection_string, ignore_warnings = FALSE)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_true(grepl("#temp", actual$status))
  expect_true(grepl("lines", actual$status))
})

test_that("SQL injection performs", {
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  test_file = file.path(test_folder, "errors_wout_injection.sql")
  
  # without injection error
  actual = try_run_SQL_file(test_file, db_connection_string, ignore_warnings = FALSE)
  expect_true(grepl("error", actual$status))
  
  # with injection success
  injection = list("$(value)" = 2, "$(label)" = "name")
  actual = try_run_SQL_file(test_file, db_connection_string, injection = injection, ignore_warnings = FALSE)
  expect_equal(actual$status, "Successful completion")
})
