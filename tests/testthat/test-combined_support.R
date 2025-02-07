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
  
  example1 = system.file("extdata", "testing", "load_control_file", "control_file_example1.xlsx", package = "IDIr")
  loaded_file = load_control_file(example1, sheet = "summary")
  
  expect_true(is.data.frame(loaded_file))
  expect_true(nrow(loaded_file) >= 2)
  expect_true(ncol(loaded_file) >= 2)
  
  example2 = system.file("extdata", "testing", "load_control_file", "control_file_example2.csv", package = "IDIr")
  loaded_file = load_control_file(example2)
  
  expect_true(is.data.frame(loaded_file))
  expect_true(nrow(loaded_file) >= 2)
  expect_true(ncol(loaded_file) >= 2)
})

## save_code_to_script(query, desc, query_path ---------------------------- ----

test_that("code files written", {
  
  # create location
  path = system.file("extdata", "testing", package = "IDIr")
  tmp_directory = file.path(path, "SQL tmp scripts")
  folder_exists_to_start = dir.exists(tmp_directory)
  dir.create(tmp_directory)
  
  file_name = "test123456789.txt"
  
  # act
  save_code_to_script("placeholder query", file_name, folder_path = tmp_directory)
  
  folder_exists = dir.exists(tmp_directory)
  file_exists = any(grepl(file_name, list.files(tmp_directory)))
  
  # tidy
  file_to_remove = dir(tmp_directory, pattern = file_name)
  file.remove(file.path(tmp_directory, file_to_remove))
  unlink(tmp_directory, recursive = TRUE)
  
  folder_exists_at_end = dir.exists(tmp_directory)
  
  # assert
  expect_true(file_exists)
  expect_true(folder_exists)
  expect_false(folder_exists_at_end)
})
