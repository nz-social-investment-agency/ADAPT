################################################################################
#' Notes
#' 
################################################################################

# setup
control_file = system.file("extdata", "testing", "summary_tool", "control_file.csv", package = "IDIr")
tbl = system.file("extdata", "testing", "summary_tool", "tbl.csv", package = "IDIr")
results = system.file("extdata", "testing", "summary_tool", "results.csv", package = "IDIr")

# load
control_file = load_control_file(control_file)
tbl = read.csv(tbl)
results = read.csv(results)

# sort
results = dplyr::tibble(results)
results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  
  actual = run_summary(control_file, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
})

test_that("output names respond to control file numbering",{
  
  tmp = control_file
  cnames = colnames(tmp)
  cnames = gsub("2", "3", cnames)
  cnames = gsub("1", "01", cnames)
  cnames[cnames == "SUM"] = "sum9"
  colnames(tmp) = cnames
  
  actual = run_summary(tmp, tbl)
  
  expected = results
  cnames = colnames(expected)
  cnames = gsub("2", "3", cnames)
  cnames = gsub("1", "01", cnames)
  cnames[cnames == "sum"] = "sum9"
  colnames(expected) = cnames
  
  expect_equal(nrow(actual), nrow(expected))
  expect_equal(ncol(actual), ncol(expected))
  
  expect_equal(colnames(actual), colnames(expected))
})

test_that("can avoid filtering out NAs", {
  
  tmp = tbl
  tmp$region[tmp$region == 3] = NA
  
  actual_filtered = run_summary(control_file, tmp, remove_na_from_groups = TRUE)
  actual_unfiltered = run_summary(control_file, tmp, remove_na_from_groups = FALSE)
  
  expect_true(ncol(actual_filtered) == ncol(actual_unfiltered))
  expect_true(nrow(actual_filtered) != nrow(actual_unfiltered))
  expect_equal(colnames(actual_filtered), colnames(actual_unfiltered))
})

test_that("dynamic formula in control file work", {
  
  tmp = control_file
  tmp$SUM[tmp$SUM == "income"] = "{ 2*income }"
  
  actual = run_summary(tmp, tbl)
  
  expected = results
  expected$sum = 2*expected$sum
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, expected))
})

test_that("debug can be written out", {
  
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){
    dir.create(tmp_dir)
  }
  initial_contents = list.files(tmp_dir)
  
  actual = run_summary(control_file, tbl, debug_folder = tmp_dir)
  
  new_contents = setdiff(list.files(tmp_dir), initial_contents)
  
  expect_true(length(new_contents) >= 1)
  expect_true(any(grepl("summary\\.R", new_contents)))
})

test_that("errors in control file prevent execution", {
  
  tmp = control_file
  tmp$count1 = "non_existant_column"
  
  expect_error(suppressWarnings(run_summary(tmp, tbl)))
})

## test examples ---------------------------------------------------------- ----

test_that("summary_dynamic_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_dynamic_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_dynamic_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_dynamic_worked_example", "results.csv", package = "IDIr")
  
  # load
  control_file = load_control_file(control_file)
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  actual = run_summary(control_file, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
})

test_that("summary_long_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_long_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_long_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_long_worked_example", "results.csv", package = "IDIr")
  
  # load
  control_file = load_control_file(control_file)
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  actual = run_summary(control_file, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
})

test_that("summary_simple_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_simple_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_simple_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_simple_worked_example", "results.csv", package = "IDIr")
  
  # load
  control_file = load_control_file(control_file)
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  results$grplabel = as.character(results$grplabel)
  results$group = as.character(results$group)
  results$group.1 = as.character(results$group.1)
  
  actual = run_summary(control_file, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
})

test_that("summary_wide_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_wide_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_wide_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_wide_worked_example", "results.csv", package = "IDIr")
  
  # load
  control_file = load_control_file(control_file)
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  results$group.1 = as.character(results$group.1)
  results$label = as.character(results$label)
  
  actual = run_summary(control_file, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
})
