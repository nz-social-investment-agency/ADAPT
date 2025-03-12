# database connection - requires SQL Server in environment
db_connection_string = "NA"

can_connect = DBI::dbCanConnect(odbc::odbc(), .connection_string = db_connection_string)

if(nchar(db_connection_string) > 5 & !can_connect){
  stop("SQL Server connection string should not be part of package")
}

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "IDIr")
  control_file = file.path(test_folder, "control_file.csv")
  control_file = IDIr:::load_control_file(control_file)
  
  control_file$folder = test_folder
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  actual = expect_output(run_pipeline(tmp_cf, sheet = NULL, db_connection_string, delay_minutes = 0.01, ignore_warnings = FALSE), "Pipeline")
  
  expected = data.frame(
    file = c("setup.sql", "calculate.sql", "fizzbuzz.R"),
    status = c("Successful completion", "Stopped with error", "Successful completion"),
    start_time = Sys.time(),
    end_time = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  expect_equal(names(actual), names(expected))
  expect_equal(actual$file, expected$file)
  for(ii in seq_len(length(expected$status))){
    expect_true(grepl(expected$status[ii], actual$status[ii], fixed = TRUE))
  }
})

## test examples ---------------------------------------------------------- ----
