# setup
control_file = system.file("extdata", "testing", "summary_tool", "control_file.csv", package = "IDIr")
tbl = system.file("extdata", "testing", "summary_tool", "tbl.csv", package = "IDIr")
results = system.file("extdata", "testing", "summary_tool", "results.csv", package = "IDIr")

# load
tbl = read.csv(tbl)
results = read.csv(results)

# sort
results = dplyr::tibble(results)
results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))

# results
tmp_dir = tempdir()
tmp_results = file.path(tmp_dir, "temp_results.csv")

# control file setup
this_control_file = load_control_file(control_file)
this_control_file$FILE = tmp_results
tmp_cf = file.path(tmp_dir, "tmp_cf.csv")

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  expect_output(run_summary(tmp_cf, sheet = NULL, tbl), "Summary")
  
  actual = read.csv(tmp_results, stringsAsFactors = FALSE)
  actual = tibble::as_tibble(actual)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("output names respond to control file numbering",{
  
  unlink(tmp_results)
  
  tmp = this_control_file
  cnames = colnames(tmp)
  cnames = gsub("2", "3", cnames)
  cnames = gsub("1", "01", cnames)
  cnames[cnames == "SUM"] = "sum9"
  colnames(tmp) = cnames

  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(tmp, tmp_cf, row.names = FALSE)
  
  expect_output(run_summary(tmp_cf, sheet = NULL, tbl), "Success")
  
  actual = read.csv(tmp_results, stringsAsFactors = FALSE)
  actual = tibble::as_tibble(actual)
  
  expected = results
  cnames = colnames(expected)
  cnames = gsub("2", "3", cnames)
  cnames = gsub("1", "01", cnames)
  cnames[cnames == "sum"] = "sum9"
  colnames(expected) = cnames
  
  expect_equal(nrow(actual), nrow(expected))
  expect_equal(ncol(actual), ncol(expected))
  
  expect_equal(colnames(actual), colnames(expected))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("can avoid filtering out NAs", {
  
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  tmp = tbl
  tmp$region[tmp$region == 3] = NA
  
  expect_output(run_summary(tmp_cf, sheet = NULL, tmp, remove_na_from_groups = TRUE))
  actual_filtered = read.csv(tmp_results, stringsAsFactors = FALSE)
  
  expect_output(run_summary(tmp_cf, sheet = NULL, tmp, remove_na_from_groups = FALSE))
  actual_unfiltered = read.csv(tmp_results, stringsAsFactors = FALSE)
  
  expect_true(ncol(actual_filtered) == ncol(actual_unfiltered))
  expect_true(nrow(actual_filtered) != nrow(actual_unfiltered))
  expect_equal(colnames(actual_filtered), colnames(actual_unfiltered))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("dynamic formula in control file work", {
  
  tmp = this_control_file
  tmp$SUM[tmp$SUM == "income"] = "{ 2*income }"
  write.csv(tmp, tmp_cf, row.names = FALSE)
  
  expect_output(run_summary(tmp_cf, sheet = NULL, tbl))
  actual = read.csv(tmp_results, stringsAsFactors = FALSE)
  actual = tibble::as_tibble(actual)
  
  expected = results
  expected$sum = 2*expected$sum
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, expected))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("debug can be written out", {
  
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){
    dir.create(tmp_dir)
  }
  initial_contents = list.files(tmp_dir)
  
  expect_output(run_summary(tmp_cf, sheet = NULL, tbl, debug_folder = tmp_dir))
  
  new_contents = setdiff(list.files(tmp_dir), initial_contents)
  
  expect_true(length(new_contents) >= 1)
  expect_true(any(grepl("summary\\.R", new_contents)))
})

test_that("errors in control file prevent execution", {
  
  tmp = this_control_file
  tmp$count10 = "non_existant_column"
  write.csv(tmp, tmp_cf, row.names = FALSE)
  
  expect_error(
    capture_output(suppressWarnings(
      run_summary(tmp_cf, sheet = NULL, tbl)
    )),
    "valid_control_file"
  )
})

## test examples ---------------------------------------------------------- ----

test_that("summary_dynamic_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_dynamic_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_dynamic_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_dynamic_worked_example", "results.csv", package = "IDIr")
  
  # control file setup
  this_control_file = load_control_file(control_file)
  this_control_file$FILE = tmp_results
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  capture_output(run_summary(tmp_cf, sheet = NULL, tbl))
  actual = read.csv(tmp_results)
  actual = tibble::as_tibble(actual)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("summary_long_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_long_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_long_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_long_worked_example", "results.csv", package = "IDIr")
  
  # control file setup
  this_control_file = load_control_file(control_file)
  this_control_file$FILE = tmp_results
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  capture_output(run_summary(tmp_cf, sheet = NULL, tbl))
  actual = read.csv(tmp_results)
  actual = tibble::as_tibble(actual)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("summary_simple_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_simple_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_simple_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_simple_worked_example", "results.csv", package = "IDIr")
  
  # control file setup
  this_control_file = load_control_file(control_file)
  this_control_file$FILE = tmp_results
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  results$grplabel = as.character(results$grplabel)
  results$group = as.character(results$group)
  results$group.1 = as.character(results$group.1)
  
  capture_output(run_summary(tmp_cf, sheet = NULL, tbl))
  actual = read.csv(tmp_results)
  actual = tibble::as_tibble(actual)
  
  actual$grplabel = as.character(actual$grplabel)
  actual$group = as.character(actual$group)
  actual$group.1 = as.character(actual$group.1)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("summary_wide_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_wide_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_wide_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_wide_worked_example", "results.csv", package = "IDIr")
  
  # control file setup
  this_control_file = load_control_file(control_file)
  this_control_file$FILE = tmp_results
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  results$group.1 = as.character(results$group.1)
  results$label = as.character(results$label)
  
  capture_output(run_summary(tmp_cf, sheet = NULL, tbl))
  actual = read.csv(tmp_results)
  actual = tibble::as_tibble(actual)
  
  actual$group.1 = as.character(actual$group.1)
  actual$label = as.character(actual$label)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})

test_that("summary_entity_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "summary_entity_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "summary_entity_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "summary_entity_worked_example", "results.csv", package = "IDIr")
  
  # control file setup
  this_control_file = load_control_file(control_file)
  this_control_file$FILE = tmp_results
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(this_control_file, tmp_cf, row.names = FALSE)
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  capture_output(run_summary(tmp_cf, sheet = NULL, tbl))
  actual = read.csv(tmp_results)
  actual = tibble::as_tibble(actual)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  expect_true(all.equal(actual, results))
  
  unlink(tmp_cf)
  unlink(tmp_results)
})
