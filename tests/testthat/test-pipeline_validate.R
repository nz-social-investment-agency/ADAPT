# database connection - requires SQL Server in environment
db_connection_string = "NA"

can_connect = DBI::dbCanConnect(odbc::odbc(), .connection_string = db_connection_string)

if(nchar(db_connection_string) > 5 & !can_connect){
  stop("SQL Server connection string should not be part of package")
}

# setup
test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
control_file = load_control_file(file.path(test_folder, "control_file.csv"))

control_file$folder = test_folder

## test passing functionality --------------------------------------------- ----

test_that("worked example passes", {
  skip_if_not(can_connect)
  
  expect_true(validate_pipeline_control_file(control_file, db_connection_string))
  expect_silent(validate_pipeline_control_file(control_file, db_connection_string))
})

## test failing functionality --------------------------------------------- ----

test_that("Required column names are present", {
  tmp = control_file
  colnames(tmp)[3] = "folder_namez"
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "folder")
})

test_that("Extra columns warn", {
  skip_if_not(can_connect)
  
  tmp = control_file
  tmp$extra_col = "hello"
  
  expect_true(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "extra_col")
})

test_that("File names are accepted extension", {
  tmp = control_file
  tmp$file[2] = gsub("\\.sql$", ".zz", tmp$file[2])
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  skip_if_not(can_connect)
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "unaccepted extension")
})

test_that("If any `.sql` files, then confirm database connection works", {
  skip_if_not(can_connect)
  
  tmp = "DRIVER=ODBC Driver 18 for SQL Server; Trusted_Connection=Yes; TrustServerCertificate=Yes;"
  
  expect_false(suppressWarnings(validate_pipeline_control_file(control_file, tmp)))
  expect_warning(validate_pipeline_control_file(control_file, tmp), "connect")
})

test_that("All folders exist", {
  tmp = control_file
  tmp$folder[2] = "non existent folder"
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  skip_if_not(can_connect)
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "non existent folder")
})

test_that("All files exist in their folders", {
  tmp = control_file
  tmp$file[2] = "non existent file.R"
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  skip_if_not(can_connect)
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "non existent file.R")
  
  tmp = control_file
  tmp_dir = normalizePath(tempdir())
  tmp$folder[2] = tmp_dir
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), tmp_dir)
})

test_that("SQL files generate no errors on parse and compile", {
  skip_if_not(can_connect)
  
  tmp = control_file
  tmp$file[1] = "calculate_w_error.sql"
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "calculate_w_error")
  expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "common table expression")
  
  tmp = control_file
  tmp$file[2] = "setup_w_error.sql"
  
  expect_false(suppressWarnings(validate_pipeline_control_file(tmp, db_connection_string)))
  suppressWarnings(expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "setup_w_error"))
  suppressWarnings(expect_warning(validate_pipeline_control_file(tmp, db_connection_string), "Incorrect syntax"))
})

## test injection --------------------------------------------------------- ----

test_that("injection performs", {
  skip_if_not(can_connect)
  
  control_file = data.frame(
    folder = test_folder,
    file = c("errors_wout_injection.sql", "errors_wout_injection.R")
  )
  
  expect_false(suppressWarnings(validate_pipeline_control_file(control_file, db_connection_string)))
  expect_warning(validate_pipeline_control_file(control_file, db_connection_string))
  
  injection = list("$(value)" = 2, "$(label)" = "name")
  expect_true(validate_pipeline_control_file(control_file, db_connection_string, injection))
})
