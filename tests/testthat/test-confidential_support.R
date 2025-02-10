################################################################################
#' Notes
#'  
################################################################################

## suppression_format_extract(string) ------------------------------------- ----

test_that("valid text works as expected", {
  
  actual = suppression_format_extract("col<1")
  expected = list(input = "col<1", column = "col", sign = "<", threshold = 1, valid = TRUE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("text_name<=10")
  expected = list(input = "text_name<=10", column = "text_name", sign = "<=", threshold = 10, valid = TRUE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("002>.name.")
  expected = list(input = "002>.name.", column = ".name.", sign = ">", threshold = 2, valid = TRUE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract(" 6 >= text ")
  expected = list(input = "6>=text", column = "text", sign = ">=", threshold = 6, valid = TRUE)
  expect_equal(actual, expected)
  
})

test_that("invalid text fails", {
  
  actual = suppression_format_extract("col < one")
  expected = list(input = "col<one", column = "col", sign = "<", threshold = NA_integer_, valid = FALSE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("text_name=10")
  expected = list(input = "text_name=10", column = NA_character_, sign = "=", threshold = NA_integer_, valid = FALSE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("rubbish text")
  expected = list(input = "rubbishtext", column = NA_character_, sign = "", threshold = NA_integer_, valid = FALSE)
  expect_equal(actual, expected)

})

## ----
