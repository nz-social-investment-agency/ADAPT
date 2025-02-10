################################################################################
#' Notes
#' 
################################################################################

# setup
control_file = system.file("extdata", "testing", "summary_tool", "control_file.csv", package = "IDIr")
tbl = system.file("extdata", "testing", "summary_tool", "tbl.csv", package = "IDIr")

# load
control_file = load_control_file(control_file)
tbl = read.csv(tbl)

## test passing functionality --------------------------------------------- ----

test_that("worked example passes",{
  
  expect_true(validate_summary_control_file(control_file, tbl))
  expect_silent(validate_summary_control_file(control_file, tbl))
  
})

## test failing functionality --------------------------------------------- ----

test_that("unaccepted columns warn",{
  
  tmp = control_file
  tmp$unusual_column_name = "c1"
  
  expect_true(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "unusual_column_name")
  
})

test_that("missing columns fail",{
  
  tmp = tbl
  tmp$uid = NULL
  
  expect_false(suppressWarnings(validate_summary_control_file(control_file, tmp)))
  expect_warning(validate_summary_control_file(control_file, tmp), "uid")
})

test_that("dynamic formula can fail",{
  
  tmp = control_file
  tmp$SUM[3] = "{2*island}"
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "Calculation .* errored")
  
})

test_that("empty columns warn",{
  
  tmp = tbl
  tmp$uid = NA
  
  expect_true(suppressWarnings(validate_summary_control_file(control_file, tmp)))
  expect_warning(validate_summary_control_file(control_file, tmp), "missing values")
  
})

test_that("non-numeric sums warn",{
  
  tmp = control_file
  tmp$SUM[1] = "island"
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "sum non-numeric")
  
})

test_that("rows without summary fail",{
  
  tmp = control_file
  tmp[1,4:ncol(tmp)] = NA
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "row with no")
  
})
