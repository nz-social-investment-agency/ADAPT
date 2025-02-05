################################################################################
#' Notes
#' 
################################################################################

## example_control_file(folder, tool) ------------------------------------- ----

test_that("summary files copied in",{
  
  expect_false(file.exists("./example_control_file_1.csv"))
  
  copied_files = example_control_file(folder = ".", tool = "summary")
  
  for(ff in copied_files){
    expect_true(file.exists(ff))
    unlink(ff)
    expect_false(file.exists(ff))
  }
})

## load_control_file(path_and_file_name) ---------------------------------- ----

test_that("summary control file loaded",{
  
  example1 = system.file("extdata", "testing", "summary", "control_file_example1.xlsx", package = "IDIr")
  loaded_file = load_control_file(example1, sheet = "summary")
  
  expect_true(is.data.frame(loaded_file))
  expect_true(nrow(loaded_file) >= 2)
  expect_true(ncol(loaded_file) >= 2)
  
  example2 = system.file("extdata", "testing", "summary", "control_file_example2.csv", package = "IDIr")
  loaded_file = load_control_file(example2)
  
  expect_true(is.data.frame(loaded_file))
  expect_true(nrow(loaded_file) >= 2)
  expect_true(ncol(loaded_file) >= 2)
})
