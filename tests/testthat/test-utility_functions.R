## "%not_in%"(x, y) ------------------------------------------------------- ----

test_that("multivariate LHS is correct", {
  not_in1 <- c(1, 2) %not_in% c(1, 2, 3)
  not_in2 <- c(8, 2) %not_in% c(1, 2, 3)
  in1 <- c(1, 2) %in% c(1, 2, 3)
  in2 <- c(8, 2) %in% c(1, 2, 3)
  
  expect_equal(not_in1, !in1)
  expect_equal(not_in2, !in2)
})

test_that("different calls work", {
  b1 <- c("a", "b", "c")
  b2 <- c("c", "d", "e")
  
  expect_equal(b1 %not_in% b2, `%not_in%`(b1, b2))
})

## run_time_inform_user(msg, log = NA_character_) ------------------------- ----

test_that("msg displayed", {
  
  expect_output(run_time_inform_user("hello there"), "hello there")
  expect_output(run_time_inform_user("hello there"), as.character(Sys.Date()))
  
})

test_that("log written", {
  
  tmp_dir = tempdir()
  log_file = file.path(tmp_dir, "log.text")
  write("", log_file)
  
  expect_output(run_time_inform_user("hello there", log_file))
  
  actual = readLines(log_file)
  
  expect_true(any(grepl("hello there", actual, fixed = TRUE)))
})

## add_delimiters(string, delimiter) -------------------------------------- ----

test_that("delimiters added", {
  expect_equal(add_delimiters("text", "[]"), "[text]")
  expect_equal(add_delimiters("[text", "[]"), "[text]")
  expect_equal(add_delimiters("text]", "[]"), "[text]")
  expect_equal(add_delimiters("[text]", "[]"), "[text]")
  
  expect_equal(add_delimiters("[odd][text]", "[]"), "[odd][text]")
  expect_equal(add_delimiters("[text]", "\""), "\"[text]\"")
  expect_equal(add_delimiters("\"text\"", "[]"), "[\"text\"]")
  
  expect_equal(add_delimiters("text", "{}"), "{text}")
  expect_equal(add_delimiters(" text ", "{}"), "{ text }")
})

test_that("array works", {
  text_in = c("a","b")
  text_out = c("[a]","[b]")
  
  expect_equal(add_delimiters(text_in, "[]"), text_out)
})

test_that("delimiters added to NA is NA", {
  text_in = c("a",NA)
  text_out = c("[a]",NA)
  
  expect_equal(add_delimiters(text_in, "[]"), text_out)
})

## remove_delimiters(string, delimiter) ----------------------------------- ----

test_that("delimiters removed", {
  expect_equal(remove_delimiters("[text]", "[]"), "text")
  expect_equal(remove_delimiters("[odd][text]", "[]"), "odd][text")
  expect_equal(remove_delimiters("[text]", "\""), "[text]")
  expect_equal(remove_delimiters("\"text\"", "[]"), "\"text\"")
  expect_equal(remove_delimiters("\"text\"", "\""), "text")
  expect_equal(remove_delimiters("text", "[]"), "text")
  expect_equal(remove_delimiters("text", "t"), "ex")
  expect_equal(remove_delimiters("[text", "[]"), "text")
  expect_equal(remove_delimiters("text]", "[]"), "text")
  
  expect_equal(remove_delimiters("{text}", "{}"), "text")
  expect_equal(remove_delimiters("{ text }", "{}"), "text")
})

## is_delimited(string, delimiter) ---------------------------------------- ----


test_that("delimiters are checked", {
  expect_true(is_delimited("[string]", "[]"))
  expect_true(is_delimited('"string"', "\""))
  expect_true(is_delimited('"[string]"', "\""))
  expect_true(is_delimited('"string"', "\""))
  expect_true(is_delimited("\"string\"", "\""))
})

test_that("delimiters are not muddled", {
  expect_false(is_delimited("[string]", "\""))
  expect_false(is_delimited('"string"', "[]"))
  expect_false(is_delimited('"[string]"', "[]"))
  expect_false(is_delimited('"string"', "[]"))
  expect_false(is_delimited("\"string\"", "[]"))
})

test_that("non-sql delimiters work", {
  expect_true(is_delimited("astringa", "a"))
  expect_true(is_delimited("astringb", "ab"))
  expect_true(is_delimited("astringb", "ab"))
  expect_false(is_delimited(" string ", "ab"))
  expect_false(is_delimited("astring ", "ab"))
  expect_false(is_delimited(" stringb", "ab"))
})

## no_obvious_escaping_injection(string) ---------------------------------- ----

test_that("innocent text accepted", {
  expect_true(no_obvious_escaping_injection("innocent text"))
  expect_true(no_obvious_escaping_injection("innocent text()"))
  expect_true(no_obvious_escaping_injection("'innocent text'"))
})

test_that("special characters rejected", {
  expect_false(no_obvious_escaping_injection("not innocent text;"))
  expect_false(no_obvious_escaping_injection("not innocent text}"))
  expect_false(no_obvious_escaping_injection("not{innocent text"))
})

test_that("unmatched quotes rejected", {
  expect_false(no_obvious_escaping_injection("not \"innocent text"))
  expect_false(no_obvious_escaping_injection("not innocent' text"))
  expect_false(no_obvious_escaping_injection("`not innocent text"))
})

test_that("unmatched brackets rejected", {
  expect_false(no_obvious_escaping_injection("not (innocent text"))
  expect_false(no_obvious_escaping_injection("not innocent text)"))
  expect_false(no_obvious_escaping_injection("not [innocent text"))
  expect_false(no_obvious_escaping_injection("not innocent text]"))
})

## sql2id(sql_string) ----------------------------------------------------- ----

test_that("single sql string to Id", {
  actual = sql2id("[name]")
  expected = DBI::Id("name")
  expect_identical(actual, expected)
})

test_that("multi sql string to Id", {
  actual = sql2id("[db].[schema].[table]")
  expected = DBI::Id("db", "schema", "table")
  expect_identical(actual, expected)
})

## readLines_utf8(file_name_and_path) ------------------------------------- ----

test_that("file read equivalent to readLines", {
  tmp_dir = tempdir()
  tmp_file = file.path(tmp_dir, "test.txt")
  writeLines("hello\nthere\nmy\nfriend", tmp_file)
  on.exit(unlink(tmp_file))
  
  # reading
  base = readLines(tmp_file)
  this = readLines_utf8(tmp_file)
  
  # match
  expect_equal(base, this)
})

test_that("non-UTF-8 character causes error", {
  tmp_file = system.file("extdata", "testing", "utf8_error.txt", package = "ADAPT")
  
  expect_silent(readLines(tmp_file))
  expect_warning(expect_error(readLines_utf8(tmp_file), "UTF-8"), "invalid input")
})

test_that("incomplete final line causes error", {
  tmp_file = system.file("extdata", "testing", "missing_eoline.sql", package = "ADAPT")
  
  expect_warning(expect_error(readLines_utf8(tmp_file), "fixed by adding"), "incomplete final line")
})

## increment_file_name(path_and_file_name) -------------------------------- ----

test_that("file numbers incremented", {
  
  tmp_dir = tempdir()
  num = floor(1e6 * stats::runif(1))
  file_name = glue::glue("test {num}.txt")
  
  file = increment_file_name(file.path(tmp_dir, file_name))
  writeLines("hello", file)
  
  expect_true(file.exists(file.path(tmp_dir, glue::glue("test {num}.txt"))))
  expect_false(file.exists(file.path(tmp_dir, glue::glue("test {num} (1).txt"))))
  
  file = increment_file_name(file.path(tmp_dir, file_name))
  writeLines("hello", file)
  
  expect_true(file.exists(file.path(tmp_dir, glue::glue("test {num} (1).txt"))))
  expect_false(file.exists(file.path(tmp_dir, glue::glue("test {num} (2).txt"))))
  
  file = increment_file_name(file.path(tmp_dir, file_name))
  writeLines("hello", file)
  
  expect_true(file.exists(file.path(tmp_dir, glue::glue("test {num} (2).txt"))))
  expect_false(file.exists(file.path(tmp_dir, glue::glue("test {num} (3).txt"))))
  
  file = increment_file_name(file.path(tmp_dir, file_name))
  writeLines("hello", file)
  
  expect_true(file.exists(file.path(tmp_dir, glue::glue("test {num} (3).txt"))))
  
  unlink(file.path(tmp_dir, glue::glue("test {num}.txt")))
  unlink(file.path(tmp_dir, glue::glue("test {num} (1).txt")))
  unlink(file.path(tmp_dir, glue::glue("test {num} (2).txt")))
  unlink(file.path(tmp_dir, glue::glue("test {num} (3).txt")))
})


