################################################################################
#' Notes
#' - testing of load_control_file outstanding
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

test_that("control file loaded",{
  
  example_to_load = system.file("extdata", "examples", "example_control_file_1.csv", package = "summaryr")
  
  actual_loaded = load_control_file(example_to_load)
  
  expected_loaded = data.frame(
    ENABLED1 = c("TRUE","TRUE","TRUE","FALSE","TRUE","TRUE","TRUE"),
    GROUP1 = c("age", NA,"age","age","age","age","age"),
    GROUP2 = c(NA,"region","region",NA,NA,NA,"region"),
    GROUP3 = c(NA,NA,NA,"enrolled",NA,"enrolled",NA),
    LABEL1 = c("num","num","num","pupils","income","income","ed_visits"),
    DISTINCT1 = c("snz_uid","snz_uid","snz_uid","snz_uid","snz_uid","snz_uid","snz_uid"),
    SUM1 = c(NA,NA,NA,NA,"income","income","ed_events"),
    ENTITY1 = c(NA,NA,NA,"school_id",NA,"school_id",NA),
    stringsAsFactors = FALSE
  )
  
  expected_loaded = dplyr::select(expected_loaded, dplyr::all_of(colnames(actual_loaded)))
  
  expect_true(all.equal(actual_loaded, expected_loaded))
})



