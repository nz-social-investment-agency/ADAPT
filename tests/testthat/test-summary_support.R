################################################################################
#' Notes
#'  
################################################################################

## generate_combinations_df(...,always,drop.dupes.within,drop.dupes.across) ----

test_that("cross product outputs", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  
  out_actual = generate_combinations_df(in1,in2)
  
  out_expected = data.frame(
    column2 = c("4","4","4","5","5","5"),
    column1 = c("1","2","3","1","2","3"),
    stringsAsFactors = FALSE
  )
  
  out_expected = dplyr::select(out_expected, dplyr::all_of(colnames(out_actual)))
  
  expect_true(all.equal(out_actual, out_expected))
})

test_that("always appears in all products", {
  in1 = c("1","2")
  in2 = c("3","4")
  in3 = c("5","6")
  
  out_actual = generate_combinations_df(in1,in2, always = in3)
  
  out_expected = data.frame(
    column1 = c("5","5","5","5"),
    column2 = c("6","6","6","6"),
    column3 = c("1","2","1","2"),
    column4 = c("3","3","4","4"),
    stringsAsFactors = FALSE
  )
  
  out_expected = dplyr::select(out_expected, dplyr::all_of(colnames(out_actual)))
  
  expect_true(all.equal(out_actual, out_expected))
})

test_that("NA values produce combinations",{
  in1 = c("a", NA)
  in2 = c("b", NA)
  
  out_actual = generate_combinations_df(in1, in2)
  
  out_expected = data.frame(
    column1 = c("a",NA,"a"),
    column2 = c("b","b",NA),
    stringsAsFactors = FALSE
  )
  
  out_expected = dplyr::select(out_expected, dplyr::all_of(colnames(out_actual)))
  
  expect_true(all.equal(out_actual, out_expected))
})

test_that("product removes dupes within", {
  in1 = c("a","b")
  in2 = c("a","c")
  
  out_dupes = generate_combinations_df(in1, in2, drop.dupes.within = FALSE)
  out_no_dupes = generate_combinations_df(in1, in2, drop.dupes.within = TRUE)
  
  expected_no_dupes = data.frame(
    column1 = c("a","b","a","b"),
    column2 = c( NA,"a","c","c"),
    stringsAsFactors = FALSE
  )
  
  expected_no_dupes = dplyr::select(expected_no_dupes, dplyr::all_of(colnames(out_no_dupes)))
  
  expect_true(all.equal(out_no_dupes, expected_no_dupes))
  
  expect_false(identical(out_dupes, out_no_dupes))
})

test_that("product removes dupes across", {
  in1 = c("a","b")
  
  out_dupes = generate_combinations_df(in1, in1, drop.dupes.across = FALSE, drop.dupes.within = FALSE)
  out_no_dupes = generate_combinations_df(in1, in1, drop.dupes.across = TRUE, drop.dupes.within = FALSE)
  
  expected_no_dupes = data.frame(
    column1 = c("a","b","b"),
    column2 = c("a","a","b"),
    stringsAsFactors = FALSE
  )
  
  expected_no_dupes = dplyr::select(expected_no_dupes, dplyr::all_of(colnames(out_no_dupes)))
  
  expect_true(all.equal(out_no_dupes, expected_no_dupes))
  
  expect_false(identical(out_dupes, out_no_dupes))
})

## cross_product_column_names(...,always,drop.dupes.within,drop.dupes.across) ----

test_that("cross product outputs", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  out_manual = list(c("1","4"),c("1","5"),c("2","4"),c("2","5"),c("3","4"),c("3","5"))
  out1 = cross_product_column_names(in1,in2)
  out2 = cross_product_column_names(in2,in1)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_manual)
  expect_setequal(out2, out_manual)
})

test_that("always appears in all products", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  in3 = c("6")
  
  out_manual = list(c("1","4","6"),c("1","5","6"),c("2","4","6"),c("2","5","6"),c("3","4","6"),c("3","5","6"))
  out1 = cross_product_column_names(in1,in2, in3)
  out2 = cross_product_column_names(in1,in2, always = in3)
  out1 = lapply(out1, sort)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_manual)
  expect_setequal(out2, out_manual)
  
  out_always = list(c("1","4","5"),c("2","4","5"),c("3","4","5"))
  out1 = cross_product_column_names(in1, always = in2)
  out2 = cross_product_column_names(always = in2, in1)
  out1 = lapply(out1, sort)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_always)
  expect_setequal(out2, out_always)
})

test_that("NA values produce combinations",{
  in1 = c("a", NA)
  in2 = c("b", NA)
  
  expected_out = list("a","b",c("a","b"))
  actual_out = cross_product_column_names(in1, in2)
  
  expect_setequal(expected_out, actual_out)
})

test_that("product removes dupes within", {
  in1 = c("a","b")
  in2 = c("a","c")
  out_dupes_manual = list(c("a","a"),c("a","b"),c("a","c"),c("b","c"))
  out_no_dupes_manual = list(c("a"),c("a","b"),c("a","c"),c("b","c"))
  
  out_dupes = cross_product_column_names(in1, in2, drop.dupes.within = FALSE)
  out_no_dupes = cross_product_column_names(in1, in2, drop.dupes.within = TRUE)
  out_dupes = lapply(out_dupes, sort)
  out_no_dupes = lapply(out_no_dupes, sort)
  
  expect_setequal(out_dupes, out_dupes_manual)
  expect_setequal(out_no_dupes, out_no_dupes_manual)
})

test_that("product removes dupes across", {
  in1 = c("a","b")
  out_dupes_manual = list(c("a","a"),c("a","b"),c("b","a"),c("b","b"))
  out_no_dupes_within_manual = list(c("a"),c("a","b"),c("b","a"),c("b"))
  out_no_dupes_across_manual = list(c("a","a"),c("b","a"),c("b","b"))
  out_no_dupes_both_manual = list(c("a"),c("b","a"),c("b"))
  
  out_dupes = cross_product_column_names(in1, in1, drop.dupes.within = FALSE, drop.dupes.across = FALSE)
  out_no_dupes_within = cross_product_column_names(in1, in1, drop.dupes.within = TRUE, drop.dupes.across = FALSE)
  out_no_dupes_across = cross_product_column_names(in1, in1, drop.dupes.within = FALSE, drop.dupes.across = TRUE)
  out_no_dupes_both = cross_product_column_names(in1, in1, drop.dupes.within = TRUE, drop.dupes.across = TRUE)
  
  expect_setequal(out_dupes, out_dupes_manual)
  expect_setequal(out_no_dupes_within, out_no_dupes_within_manual)
  expect_setequal(out_no_dupes_across, out_no_dupes_across_manual)
  expect_setequal(out_no_dupes_both, out_no_dupes_both_manual)
})

## generate_summary_commands(summary_row) --------------------------------- ----

test_that("individual summary commands run",{
  
  # distinct
  summary_row = data.frame(DISTINCT2 = "col", stringsAsFactors = FALSE)
  expected = c(distinct2 = "dplyr::n_distinct(col, na.rm = TRUE)")
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
  
  # count
  summary_row = data.frame(COUNT05 = "col", stringsAsFactors = FALSE)
  expected = c(count05 = "sum(ifelse(!is.na(col), 1, 0), na.rm = TRUE)")
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
  
  # sum
  summary_row = data.frame(sum = "col", stringsAsFactors = FALSE)
  expected = c(sum = "sum(col, na.rm = TRUE)")
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
  
  # entity
  summary_row = data.frame(ENTITY1 = "col", stringsAsFactors = FALSE)
  expected = c(entity1 = "dplyr::n_distinct(col, na.rm = TRUE)")
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
  
  # stddev
  summary_row = data.frame(STDDEV3 = "col", stringsAsFactors = FALSE)
  expected = c(stddev3 = "sd(col, na.rm = TRUE)")
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
  
})

test_that("multiple summary commands run",{
  
  summary_row = data.frame(
    DISTINCT1 = "col1",
    DISTINCT2 = "col2",
    COUNT05 = "col3",
    stringsAsFactors = FALSE
  )
  expected = c(
    distinct1 = "dplyr::n_distinct(col1, na.rm = TRUE)",
    distinct2 = "dplyr::n_distinct(col2, na.rm = TRUE)",
    count05 = "sum(ifelse(!is.na(col3), 1, 0), na.rm = TRUE)"
  )
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
})

test_that("NAs skipped",{
  
  summary_row = data.frame(
    DISTINCT1 = "col1",
    DISTINCT2 = NA,
    COUNT05 = "col3",
    stringsAsFactors = FALSE
  )
  expected = c(
    distinct1 = "dplyr::n_distinct(col1, na.rm = TRUE)",
    count05 = "sum(ifelse(!is.na(col3), 1, 0), na.rm = TRUE)"
  )
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
})

test_that("additional columns ignored",{
  
  summary_row = data.frame(
    DISTINCT1 = "col1",
    random_col_name = "col2",
    COUNT05 = "col3",
    stringsAsFactors = FALSE
  )
  expected = c(
    distinct1 = "dplyr::n_distinct(col1, na.rm = TRUE)",
    count05 = "sum(ifelse(!is.na(col3), 1, 0), na.rm = TRUE)"
  )
  
  actual = generate_summary_commands(summary_row)
  expect_equal(actual, expected)
})
