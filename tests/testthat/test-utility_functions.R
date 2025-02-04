################################################################################
#' Notes
#' 
################################################################################

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
