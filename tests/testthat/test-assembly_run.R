################################################################################
#' Notes
#' 
################################################################################

# testing folder
test_folder = system.file("extdata", "testing", "assembly_tool", package = "IDIr")

# setup control file
control_file = file.path(test_folder, "control_file.csv")
control_file = load_control_file(control_file)
control_file$measure_table = gsub("[IDI_Sandpit].[DL-MAA20XX-YY].", "", control_file$measure_table, fixed = TRUE)
control_file$period_start = gsub("{ DATEADD(YEAR, -1, [start_date]) }", "{ DATE([start_date], '-1 years') }", control_file$period_start, fixed = TRUE)

# setup database
db_path = file.path(test_folder, "testing_sqlite.db")
db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
# on.exit(DBI::dbDisconnect(db_connection))

# constant arguments
master_table = "tmp_master_table"

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  # Arrange
  
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){
    dir.create(tmp_dir)
  }
  initial_contents = list.files(tmp_dir)
  
  
  
  
  
  # Act
  run_assembly(control_file, db_connection, master_table, debug_folder = tmp_dir)
  
  actual = dplyr::tbl(db_connection, master_table)
  actual = as.data.frame(dplyr::collect(actual))
  
  results = file.path(test_folder, "results.csv")
  results = read.csv(results)
  
  # Assert
  new_contents = setdiff(list.files(tmp_dir), initial_contents)
  expect_true(length(new_contents) >= 1)
  expect_true(any(grepl("assembly\\.sql", new_contents)))
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  expect_true(all(colnames(actual) %in% colnames(results)))
  expect_true(all(colnames(results) %in% colnames(actual)))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual$payment_within_period = round(actual$payment_within_period, 2)
  
  expect_equal(actual, results)
})

## test examples ---------------------------------------------------------- ----


