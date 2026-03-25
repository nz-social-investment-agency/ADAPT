# database connection - requires SQL Server in environment
db_connection_string = "NA"

can_connect = DBI::dbCanConnect(odbc::odbc(), .connection_string = db_connection_string)

if(nchar(db_connection_string) > 5 & !can_connect){
  stop("SQL Server connection string should not be part of package")
}

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  control_file = file.path(test_folder, "control_file.csv")
  control_file = ADAPT:::load_control_file(control_file)
  
  control_file$folder = test_folder
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  actual = expect_output(run_pipeline(tmp_cf, sheet = NULL, db_connection_string, delay_minutes = 0.01, ignore_warnings = FALSE), "Pipeline")
  
  expected = data.frame(
    enabled = TRUE,
    order = c(1,2,3, 3.1),
    folder = test_folder,
    file = c("setup.sql", "calculate.sql", "fizzbuzz.R", "STOP IF ANY FAILURES"),
    start_time = Sys.time(),
    end_time = Sys.time(),
    status = c("Successful completion", "Stopped with error", "Successful completion", "Stopped with error"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(names(actual), names(expected))
  expect_equal(actual$file, expected$file)
  for(ii in seq_len(length(expected$status))){
    expect_true(grepl(expected$status[ii], actual$status[ii], fixed = TRUE))
  }
})

test_that("sink captures output",{
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  control_file = file.path(test_folder, "control_file.csv")
  control_file = ADAPT:::load_control_file(control_file)
  
  control_file$folder = test_folder
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  tmp_sink = file.path(tempdir(), "tmp_sink.txt")
  unlink(tmp_sink)
  
  expect_output(run_pipeline(tmp_cf, sheet = NULL, db_connection_string, delay_minutes = 0.01, ignore_warnings = FALSE, sink_file = tmp_sink))
  
  actual = readLines(tmp_sink)
  
  expect_true(grepl("==", actual[1]))
  expect_true(grepl("waiting", actual[2]))
  expect_true(grepl("sleep", actual[3]))
  expect_true(grepl("sleep", actual[4]))
  expect_true(grepl("setup\\.sql", actual[5]))
  expect_true(grepl("setup\\.sql", actual[6]))
  expect_true(grepl("calculate\\.sql", actual[7]))
  expect_true(grepl("calculate\\.sql", actual[8]))
  expect_true(grepl("error", actual[8]))
  expect_true(grepl("fizzbuzz.R", actual[20]))
  expect_true(grepl("fizzbuzz.R", actual[21]))
  expect_true(grepl("verifying", actual[22]))
  expect_true(grepl("stop with failures", actual[23]))
  expect_true(grepl("complete", actual[24]))
  
  unlink(tmp_sink)
})

test_that("sink captures stderr",{
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  control_file = file.path(test_folder, "control_file.csv")
  control_file = ADAPT:::load_control_file(control_file)
  control_file$file[3] = "fizzbuzz_w_message.R"
  
  control_file$folder = test_folder
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  tmp_sink = file.path(tempdir(), "tmp_sink.txt")
  unlink(tmp_sink)
  
  expect_output(run_pipeline(tmp_cf, sheet = NULL, db_connection_string, delay_minutes = 0.01, ignore_warnings = FALSE, sink_file = tmp_sink))
  
  actual = readLines(tmp_sink)
  expect_true(any(grepl("Global message", actual)))

  unlink(tmp_sink)
})

## test examples ---------------------------------------------------------- ----

## test injection --------------------------------------------------------- ----

test_that("injection performs", {
  skip_if_not(can_connect)
  
  test_folder = system.file("extdata", "testing", "pipeline_tool", package = "ADAPT")
  control_file = data.frame(
    folder = test_folder,
    file = c("errors_wout_injection.sql", "errors_wout_injection.R")
  )
  
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(control_file, tmp_cf, row.names = FALSE)
  
  injection_sql = list("$(value)" = 2, "$(label)" = "name")
  injection_r = list(my_value = 10)
  
  # no injection errors in SQL
  expect_error(suppressWarnings(capture_output(
    run_pipeline(tmp_cf, db_connection_string = db_connection_string, delay_minutes = 0)
  )), "valid_control_file")
  
  # providing SQL injection errors in R execution
  tmp = capture_output({
    results = run_pipeline(tmp_cf, db_connection_string = db_connection_string, delay_minutes = 0, injection_sql = injection_sql)
  })
  expect_true(grepl("error", results$status[2], fixed = TRUE))
  
  # providing both injection runs successfully
  tmp = capture_output({
    results = run_pipeline(tmp_cf, db_connection_string = db_connection_string, delay_minutes = 0, injection_sql = injection_sql, injection_r = injection_r)
  })
  expect_true(all(grepl("successful", results$status, ignore.case = TRUE)))
})
