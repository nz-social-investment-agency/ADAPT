## database setup --------------------------------------------------------- ----
# Creates SQLite database for testing assembly tool
# For SQLite, store dates as text in YYYY-MM-DD format

required_packages = c("DBI", "RSQLite")
stopifnot(all(required_packages %in% installed.packages()))

make_sqlite_db_for_testing = function(csv_path, db_path){
  # delete old database
  unlink(db_path)
  db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_conn), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  # write to database supporting function
  copy_r_to_sql = function(db_connection, sql_table, r_table) {
    stopifnot("tbl_sql" %not_in% class(r_table))
    # copy data - mute any translation message
    suppressMessages(DBI::dbWriteTable(db_connection, DBI::Id(table = sql_table), r_table))
  }
  
  # get csv files to process
  csv_files = list.files(csv_path, "\\.csv")
  csv_files = setdiff(csv_files, c("control_file.csv", "results.csv"))
  
  # load csv files to db
  for(cc in csv_files){
    this_csv = read.csv(file.path(csv_path, cc), stringsAsFactors = FALSE)
    
    sql_table_name = gsub("^data_", "tmp_", cc)
    sql_table_name = gsub("\\.csv", "", sql_table_name)
    
    copy_r_to_sql(db_conn, sql_table = sql_table_name, r_table = this_csv)
  }
  
  return(invisible(length(csv_files)))
}

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  ### Arrange ----
  
  # paths
  test_folder = system.file("extdata", "testing", "assembly_tool", package = "ADAPT")
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){ dir.create(tmp_dir) }
  db_path = file.path(tmp_dir, "testing_sqlite.db")
  
  # setup database
  created_tables = make_sqlite_db_for_testing(test_folder, db_path)
  stopifnot(created_tables == 3)
  db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  # setup control file
  control_file = file.path(test_folder, "control_file.csv")
  control_file = load_control_file(control_file)
  control_file$measure_table = gsub("[IDI_Sandpit].[DL-MAA20XX-YY].", "", control_file$measure_table, fixed = TRUE)
  control_file$period_start = gsub("{ DATEADD(YEAR, -1, [start_date]) }", "{ DATE([start_date], '-1 years') }", control_file$period_start, fixed = TRUE)
  
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  # constant arguments
  master_table = "tmp_master_table"
  
  ### Act ----
  initial_contents = list.files(tmp_dir)
  expect_output(run_assembly(tmp_cf, sheet = NULL, db_connection, master_table, debug_folder = tmp_dir), "Assembly")
  
  actual = dplyr::tbl(db_connection, master_table)
  actual = as.data.frame(dplyr::collect(actual))
  
  results = file.path(test_folder, "results.csv")
  results = read.csv(results)
  
  ### Assert ----
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
  
  output_cf = load_control_file(tmp_cf)
  expect_true(all(c("start_time", "end_time", "status") %in% colnames(output_cf)))
})

## test examples ---------------------------------------------------------- ----

test_that("assembly_mother_child_worked_example passes", {
  ### Arrange ----
  
  # paths
  test_folder = system.file("extdata", "examples", "assembly_mother_child_worked_example", package = "ADAPT")
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){ dir.create(tmp_dir) }
  db_path = file.path(tmp_dir, "testing_sqlite.db")
  
  # setup database
  created_tables = make_sqlite_db_for_testing(test_folder, db_path)
  stopifnot(created_tables == 3)
  db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  # setup control file
  control_file = file.path(test_folder, "control_file.csv")
  control_file = load_control_file(control_file)
  control_file$measure_table = gsub("[IDI_Sandpit].[DL-MAA20XX-YY].", "", control_file$measure_table, fixed = TRUE)
  control_file$period_start = gsub("{ DATEADD(YEAR, -1, [start_date]) }", "{ DATE([start_date], '-1 years') }", control_file$period_start, fixed = TRUE)
  
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  # constant arguments
  master_table = "tmp_master_table"
  
  ### Act ----
  initial_contents = list.files(tmp_dir)
  expect_output(run_assembly(tmp_cf, sheet = NULL, db_connection, master_table, debug_folder = tmp_dir), "Assembly")
  
  actual = dplyr::tbl(db_connection, master_table)
  actual = as.data.frame(dplyr::collect(actual))
  
  results = file.path(test_folder, "results.csv")
  results = read.csv(results)
  
  ### Assert ----
  new_contents = setdiff(list.files(tmp_dir), initial_contents)
  expect_true(length(new_contents) >= 1)
  expect_true(any(grepl("assembly\\.sql", new_contents)))
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  expect_true(all(colnames(actual) %in% colnames(results)))
  expect_true(all(colnames(results) %in% colnames(actual)))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))

  expect_equal(actual, results)
})

test_that("assembly_panel_worked_example passes", {
  ### Arrange ----
  
  # paths
  test_folder = system.file("extdata", "examples", "assembly_panel_worked_example", package = "ADAPT")
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){ dir.create(tmp_dir) }
  db_path = file.path(tmp_dir, "testing_sqlite.db")
  
  # setup database
  created_tables = make_sqlite_db_for_testing(test_folder, db_path)
  stopifnot(created_tables == 3)
  db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  # setup control file
  control_file = file.path(test_folder, "control_file.csv")
  control_file = load_control_file(control_file)
  control_file$measure_table = gsub("[IDI_Sandpit].[DL-MAA20XX-YY].", "", control_file$measure_table, fixed = TRUE)
  control_file$period_start = gsub("{ DATEADD(YEAR, -1, [start_date]) }", "{ DATE([start_date], '-1 years') }", control_file$period_start, fixed = TRUE)
  
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  # constant arguments
  master_table = "tmp_master_table"
  
  ### Act ----
  initial_contents = list.files(tmp_dir)
  expect_output(run_assembly(tmp_cf, sheet = NULL, db_connection, master_table, debug_folder = tmp_dir), "Assembly")
  
  actual = dplyr::tbl(db_connection, master_table)
  actual = as.data.frame(dplyr::collect(actual))
  
  results = file.path(test_folder, "results.csv")
  results = read.csv(results)
  
  ### Assert ----
  new_contents = setdiff(list.files(tmp_dir), initial_contents)
  expect_true(length(new_contents) >= 1)
  expect_true(any(grepl("assembly\\.sql", new_contents)))
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  expect_true(all(colnames(actual) %in% colnames(results)))
  expect_true(all(colnames(results) %in% colnames(actual)))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual$imports = round(actual$imports, 2)
  actual$exports = round(actual$exports, 2)
  
  expect_equal(actual, results)
})

test_that("assembly_sequence_worked_example passes", {
  ### Arrange ----
  
  # paths
  test_folder = system.file("extdata", "examples", "assembly_sequence_worked_example", package = "ADAPT")
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){ dir.create(tmp_dir) }
  db_path = file.path(tmp_dir, "testing_sqlite.db")
  
  # setup database
  created_tables = make_sqlite_db_for_testing(test_folder, db_path)
  stopifnot(created_tables == 2)
  db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_connection))
  
  # setup control file
  control_file = file.path(test_folder, "control_file.csv")
  control_file = load_control_file(control_file)
  control_file$measure_table = gsub("[IDI_Sandpit].[DL-MAA20XX-YY].", "", control_file$measure_table, fixed = TRUE)
  control_file$period_start = gsub("{ DATEADD(YEAR, -1, [start_date]) }", "{ DATE([start_date], '-1 years') }", control_file$period_start, fixed = TRUE)
  
  tmp_cf = file.path(tmp_dir, "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  # constant arguments
  master_table = "tmp_master_table"
  
  ### Act ----
  initial_contents = list.files(tmp_dir)
  expect_output(run_assembly(tmp_cf, sheet = NULL, db_connection, master_table, debug_folder = tmp_dir), "Assembly")
  
  actual = dplyr::tbl(db_connection, master_table)
  actual = as.data.frame(dplyr::collect(actual))
  
  results = file.path(test_folder, "results.csv")
  results = read.csv(results)
  
  ### Assert ----
  new_contents = setdiff(list.files(tmp_dir), initial_contents)
  expect_true(length(new_contents) >= 1)
  expect_true(any(grepl("assembly\\.sql", new_contents)))
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  expect_true(all(colnames(actual) %in% colnames(results)))
  expect_true(all(colnames(results) %in% colnames(actual)))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))

  expect_equal(actual, results)
})

## try_run_SQL_query(query, db_connection, ignore_warnings = FALSE) ------- ----

test_that("query executes successfully", {
  # paths
  test_folder = system.file("extdata", "testing", "assembly_tool", package = "ADAPT")
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){ dir.create(tmp_dir) }
  db_path = file.path(tmp_dir, "testing_sqlite.db")
  
  # setup database
  created_tables = make_sqlite_db_for_testing(test_folder, db_path)
  stopifnot(created_tables == 3)
  db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  test_query = "SELECT snz_uid FROM tmp_master_table"
  actual = try_run_SQL_query(test_query, db_connection)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_equal(actual$status, "Successful completion")
})

test_that("query failure captured", {
  # paths
  test_folder = system.file("extdata", "testing", "assembly_tool", package = "ADAPT")
  tmp_dir = tempdir()
  if(!dir.exists(tmp_dir)){ dir.create(tmp_dir) }
  db_path = file.path(tmp_dir, "testing_sqlite.db")
  
  # setup database
  created_tables = make_sqlite_db_for_testing(test_folder, db_path)
  stopifnot(created_tables == 3)
  db_connection = DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db_connection), add = TRUE, after = TRUE)
  on.exit(unlist(db_path), add = TRUE, after = TRUE)
  
  test_query = "SELECT snz_uid FROM non_existant_table"
  actual = try_run_SQL_query(test_query, db_connection)
  
  expect_equal(names(actual), c("status", "start_time", "end_time"))
  expect_true(grepl("Stopped with error", actual$status, fixed = TRUE))
  expect_true(grepl("non_existant_table", actual$status, fixed = TRUE))
})
