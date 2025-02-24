################################################################################
#' Notes
#' 
################################################################################

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

## setup ------------------------------------------------------------------ ----

# paths
test_folder = system.file("extdata", "testing", "assembly_tool", package = "IDIr")
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

# constant arguments
master_table = "tmp_master_table"
sql_folder = test_folder

## test passing functionality --------------------------------------------- ----

test_that("worked example passes",{
  
  expect_true(validate_assembly_control_file(control_file, db_connection, master_table, sql_folder))
  expect_silent(validate_assembly_control_file(control_file, db_connection, master_table, sql_folder))
  
})

test_that("table in file passes", {
  tmp = control_file
  tmp$measure_table[3] = "[table_within_file]"
  
  expect_true(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder))
  expect_silent(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder))
})

test_that("column in file passes", {
  
  # column not in table but table in database > check database > fail
  tmp = control_file
  tmp$measure_start[8] = "[column_within_file]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "found in table")
  
  # column not in table and table not in database > check file > pass
  tmp$measure_table[8] = "[table_within_file]"
  
  expect_true(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder))
  expect_silent(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder))
  
  # column not in file and table not in database > check file > fail
  tmp$measure_start[8] = "[invalid_column_within_file]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "found in file")
})

## test failing functionality --------------------------------------------- ----

test_that("missing columns fail", {
  
  tmp = dplyr::rename(control_file, unusual_name = population_uid)
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "population_uid")
})

test_that("extra columns warn", {
  tmp = control_file
  tmp$unusual_name = "c1"
  
  expect_true(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "unusual_name")
})

test_that("unexpected delimiters fail", {
  tmp = control_file
  tmp$period_start[1] = remove_delimiters(tmp$period_start[1], "[]")
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "period_start")
  
  tmp = control_file
  tmp$measure_uid[10] = add_delimiters(remove_delimiters(tmp$measure_uid[10], "[]"), "\"")
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "measure_uid")
})

test_that("escaping code injection fails", {
  tmp = control_file
  tmp$measure_value[1] = "{ command;command }"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table), "escaping code injection")
})

test_that("unavailable master table fails", {
  expect_false(suppressWarnings(validate_assembly_control_file(control_file, db_connection, "invalid_master_table", sql_folder)))
  expect_warning(validate_assembly_control_file(control_file, db_connection, "invalid_master_table", sql_folder), "master[ _]table")
})

test_that("unavailable master table columsn fails", {
  tmp = control_file
  tmp$period_start[1] = "[invalid_column_name]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "invalid_column_name")
})

test_that("unavailable measure table fails", {
  tmp = control_file
  tmp$measure_table[3] = "[invalid_table_name]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "invalid_table_name")
})

test_that("unavailable measure table columns fails", {
  tmp = control_file
  tmp$measure_uid[4] = "[invalid_column_name]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "invalid_column_name")
  
  tmp = control_file
  tmp$measure_start[5] = "[invalid_column_name]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "invalid_column_name")
  
  tmp = control_file
  tmp$measure_end[6] = "[invalid_column_name]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "invalid_column_name")
  
  tmp = control_file
  tmp$measure_value[7] = "[invalid_column_name]"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "invalid_column_name")
})

test_that("invalid output method fails", {
  tmp = control_file
  tmp$output_method[8] = "INVALID"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "method")
})

test_that("invalid data types fails", {
  tmp = control_file
  tmp$output_type[9] = "INVALID"
  
  expect_false(suppressWarnings(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder)))
  expect_warning(validate_assembly_control_file(tmp, db_connection, master_table, sql_folder), "type")  
})
