################################################################################
#' Notes
#' 
################################################################################

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

## no_obvious_injection(string) ------------------------------------------- ----

test_that("innocent text accepted", {
  expect_true(no_obvious_injection("innocent text"))
  expect_true(no_obvious_injection("innocent text()"))
  expect_true(no_obvious_injection("'innocent text'"))
})

test_that("special characters rejected", {
  expect_false(no_obvious_injection("not innocent text;"))
  expect_false(no_obvious_injection("not innocent text}"))
  expect_false(no_obvious_injection("not{innocent text"))
})

test_that("unmatched quotes rejected", {
  expect_false(no_obvious_injection("not \"innocent text"))
  expect_false(no_obvious_injection("not innocent' text"))
  expect_false(no_obvious_injection("`not innocent text"))
})

test_that("unmatched brackets rejected", {
  expect_false(no_obvious_injection("not (innocent text"))
  expect_false(no_obvious_injection("not innocent text)"))
  expect_false(no_obvious_injection("not [innocent text"))
  expect_false(no_obvious_injection("not innocent text]"))
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
