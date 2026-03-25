## provide_example(example, folder) --------------------------------------- ----

test_that("available exampels listed", {
  
  expect_message(provide_example())
  
  expect_message(provide_example("made up example"))
})

test_that("summary files copied in",{
  
  tmp = file.path(tempdir(),"examples")
  
  unlink(tmp, TRUE)
  expect_true(!dir.exists(tmp))
  
  copied_files = provide_example("summary_simple_worked_example", folder = tmp)
  
  for(ff in copied_files){
    expect_true(file.exists(ff))
    unlink(ff)
    expect_false(file.exists(ff))
  }
})

## adjust_file_path_handling(file_name_and_path) -------------------------- ----

test_that("simple paths updated", {
  expected = "folder/path/here"
  
  actual = adjust_file_path_handling("folder\\path\\here")
  expect_equal(actual, expected)
  
  actual = adjust_file_path_handling("folder\\\\path\\\\here")
  expect_equal(actual, expected)
})

test_that("MAA and IMR paths have prefixes changed", {
  
  actual = adjust_file_path_handling("I:\\MAA\\MAA2020-20\\folder")
  expected = "/mnt/DataLab/MAA/MAA2020-20/folder"
  expect_equal(actual, expected)
  
  actual = adjust_file_path_handling("I:\\IMR\\IMR2020-20\\folder")
  expected = "/mnt/DataLab/IMR/IMR2020-20/folder"
  expect_equal(actual, expected)
})

## load_control_file(path_and_file_name) ---------------------------------- ----

test_that("summary control file loaded",{
  
  example1 = system.file("extdata", "testing", "load_control_file", "control_file_example1.xlsx", package = "ADAPT")
  loaded_file = load_control_file(example1, sheet = "summary")
  
  expect_true(is.data.frame(loaded_file))
  expect_true(nrow(loaded_file) >= 2)
  expect_true(ncol(loaded_file) >= 2)
  
  example2 = system.file("extdata", "testing", "load_control_file", "control_file_example2.csv", package = "ADAPT")
  loaded_file = load_control_file(example2)
  
  expect_true(is.data.frame(loaded_file))
  expect_true(nrow(loaded_file) >= 2)
  expect_true(ncol(loaded_file) >= 2)
  expect_true(is.numeric(loaded_file$ORDER))
})

## save_control_file_w_progress(path_and_file_name, sheet, progress_df, overwrite) ----

test_that("csv worked example runs", {
  # Arrange
  test_folder = system.file("extdata", "testing", "save_control_file", package = "ADAPT")
  tmp_dir = tempdir()
  
  if(file.exists(file.path(tmp_dir, "control_file.csv"))){
    unlink(file.path(tmp_dir, "control_file.csv"))
  }
  
  
  if(file.exists(file.path(tmp_dir, "control_file (1).csv"))){
    unlink(file.path(tmp_dir, "control_file (1).csv"))
  }
  
  # Act
  file.copy(file.path(test_folder, "control_file.csv"), file.path(tmp_dir, "control_file.csv"))
  progress_df = read.csv(file.path(test_folder, "progress_csv.csv"))
  
  capture_output({actual = save_control_file_w_progress(file.path(tmp_dir, "control_file.csv"), NULL, progress_df, overwrite = FALSE)})
  
  # Assess
  
  actual_cf = read.csv(actual, stringsAsFactors = FALSE, colClasses = "character")
  expected_cf = read.csv(file.path(test_folder, "expected_csv.csv"), stringsAsFactors = FALSE, colClasses = "character")

  expect_equal(actual, file.path(tmp_dir, "control_file (1).csv"))
  expect_equal(actual_cf, expected_cf)
})

test_that("xlsx worked example runs", {
  # Arrange
  test_folder = system.file("extdata", "testing", "save_control_file", package = "ADAPT")
  tmp_dir = tempdir()
  
  if(file.exists(file.path(tmp_dir, "control_file.xlsx"))){
    unlink(file.path(tmp_dir, "control_file.xlsx"))
  }
  
  
  if(file.exists(file.path(tmp_dir, "control_file (1).xlsx"))){
    unlink(file.path(tmp_dir, "control_file (1).xlsx"))
  }
  
  # Act
  file.copy(file.path(test_folder, "control_file.xlsx"), file.path(tmp_dir, "control_file.xlsx"))
  progress_df = read.csv(file.path(test_folder, "progress_xlsx.csv"))
  
  capture_output({actual = save_control_file_w_progress(file.path(tmp_dir, "control_file.xlsx"), "assembly", progress_df, overwrite = FALSE)})
  
  # Assess
  
  actual_cf = load_control_file(actual, sheet = "assembly")
  expected_cf = read.csv(file.path(test_folder, "expected_xlsx.csv"), stringsAsFactors = FALSE, colClasses = "character")
  
  for(cc in colnames(expected_cf)){
    expected_cf[[cc]] = ifelse(nchar(expected_cf[[cc]]) == 0, NA, expected_cf[[cc]])
  }
  
  expect_equal(actual, file.path(tmp_dir, "control_file (1).xlsx"))
  expect_equal(actual_cf, expected_cf)
})

## merge_progress_into_control_file(control_file, progress_df) ------------ ----

test_that("empty control file updated", {
  
  t1 = as.character(Sys.time())
  
  control_file = data.frame(
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    stringsAsFactors = FALSE
  )
  
  progress_df = data.frame(
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    start_time = rep(t1, 6),
    end_time = rep(t1, 6),
    status = c("Success", "Error", "Warning", "Success", "Error", "Warning"),
    stringsAsFactors = FALSE
  )
  
  actual = merge_progress_into_control_file(control_file, progress_df)
  
  expected = data.frame(
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    start_time = rep(t1, 6),
    end_time = rep(t1, 6),
    status = c("Success", "Error", "Warning", "Success", "Error", "Warning"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("enabled controls update", {
  
  t1 = as.character(Sys.time())
  
  control_file = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    stringsAsFactors = FALSE
  )
  
  progress_df = data.frame(
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    start_time = rep(t1, 6),
    end_time = rep(t1, 6),
    status = c("Success", "Error", "Warning", "Success", "Error", "Warning"),
    stringsAsFactors = FALSE
  )
  
  actual = merge_progress_into_control_file(control_file, progress_df)
  
  expected = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    start_time = c(t1, NA, t1, NA, t1, NA),
    end_time = c(t1, NA, t1, NA, t1, NA),
    status = c("Success", NA, "Warning", NA, "Error", NA),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("existing progress updated", {
  
  t1 = as.character(Sys.time())
  Sys.sleep(1)
  t2 = as.character(Sys.time())
  
  control_file = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    start_time = rep(t1, 6),
    end_time = rep(t1, 6),
    status = rep("Error", 6),
    stringsAsFactors = FALSE
  )
  
  progress_df = data.frame(
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    start_time = rep(t2, 6),
    end_time = rep(t2, 6),
    status = c("Success", "Error", "Warning", "Success", "Error", "Warning"),
    stringsAsFactors = FALSE
  )
  
  actual = merge_progress_into_control_file(control_file, progress_df)
  
  expected = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    start_time = c(t2, t1, t2, t1, t2, t1),
    end_time = c(t2, t1, t2, t1, t2, t1),
    status = c("Success", "Error", "Warning", "Error", "Error", "Error"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("absent progress overwrites", {
  
  t1 = as.character(Sys.time())
  Sys.sleep(1)
  t2 = as.character(Sys.time())
  
  control_file = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    start_time = rep(t1, 6),
    end_time = rep(t1, 6),
    status = rep("Error", 6),
    stringsAsFactors = FALSE
  )
  
  progress_df = data.frame(
    col1 = c(1),
    col2 = c("a"),
    start_time = t2,
    end_time = t2,
    status = "Success",
    stringsAsFactors = FALSE
  )
  
  actual = merge_progress_into_control_file(control_file, progress_df)
  
  expected = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    col1 = c(1,1,2,2,3,3),
    col2 = c("a","b","a","b","a","b"),
    col3 = 1:6,
    start_time = c(t2, t1, NA, t1, NA, t1),
    end_time = c(t2, t1, NA, t1, NA, t1),
    status = c("Success", "Error", NA, "Error", NA, "Error"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("out of order control file saves in order", {
  
  control_file = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    order = c(3,2,1,6,4,5),
    col2 = c("a","b","a","b","a","b"),
    stringsAsFactors = FALSE
  )
  
  progress_df = data.frame(
    enabled = c(TRUE, TRUE, TRUE),
    order = c(1,3,4),
    col2 = c("a","a","a"),
    start_time = c(11,22,33),
    end_time = c(12,23,34),
    status = "Success",
    stringsAsFactors = FALSE
  )
  
  actual = merge_progress_into_control_file(control_file, progress_df)
  
  expected = data.frame(
    enabled = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    order = c(3,2,1,6,4,5),
    col2 = c("a","b","a","b","a","b"),
    start_time = c('22',NA,'11',NA,'33',NA),
    end_time = c('23',NA,'12',NA,'34',NA),
    status = c("Success", NA, "Success", NA, "Success", NA),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

## save_code_to_script(query, desc, query_path ---------------------------- ----

test_that("code files written", {
  
  # create location
  path = system.file("extdata", "testing", package = "ADAPT")
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

## tolower_colnames(tbl) -------------------------------------------------- ----

test_that("in-memory columns renamed", {
  tbl = data.frame(
    COL1 = 1,
    col2 = 2,
    Col3 = 3
  )
  
  out = tolower_colnames(tbl)
  
  expect_equal(colnames(out), tolower(colnames(tbl)))
  expect_false(all(colnames(out) == colnames(tbl)))
})

test_that("remote columns renamed", {
  # Arrange
  required_packages = c("DBI", "RSQLite")
  stopifnot(all(required_packages %in% installed.packages()))
  
  db_path = file.path(tempdir(), "testing_sqlite.db")
  
  unlink(db_path)
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  tbl = data.frame(
    COL1 = 1,
    col2 = 2,
    Col3 = 3
  )
  suppressMessages(DBI::dbWriteTable(db_conn, "remote_tbl", tbl))
  
  remote_tbl = dplyr::tbl(db_conn, "remote_tbl")
  
  # Act
  renamed_remote_tbl = tolower_colnames(remote_tbl)

  # Assert
  expect_equal(colnames(renamed_remote_tbl), tolower(colnames(remote_tbl)))
  expect_false(all(colnames(renamed_remote_tbl) == colnames(remote_tbl)))
})

test_that("non-unique columns error", {
  tbl = data.frame(
    x = 1,
    X = 2
  )
  
  expect_error(tolower_colnames(tbl), "capital")
})
