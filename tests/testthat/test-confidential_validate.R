# setup
control_file = system.file("extdata", "testing", "confidential_tool", "control_file.csv", package = "IDIr")
tbl = system.file("extdata", "testing", "confidential_tool", "tbl.csv", package = "IDIr")

# load
control_file = load_control_file(control_file)
tbl = read.csv(tbl)

## test passing functionality --------------------------------------------- ----

test_that("worked example passes", {
  
  expect_true(validate_confidential_control_file(control_file, tbl))
  expect_silent(validate_confidential_control_file(control_file, tbl))
  
})

test_that("removal of confidentiality instruction passes", {
  
  for(ii in 1:nrow(control_file)){
    tmp = control_file[-ii,]
    expect_true(validate_confidential_control_file(control_file, tmp))
    expect_silent(validate_confidential_control_file(control_file, tmp))
  }

})

test_that("worked example passes without seed", {
  
  tmp = dplyr::filter(control_file, COMMAND != "SEED")
  
  expect_true(validate_confidential_control_file(control_file, tmp))
  expect_silent(validate_confidential_control_file(control_file, tmp))
  
})

## test failing functionality --------------------------------------------- ----

test_that("First column contains confidentiality commands", {
  
  tmp = control_file
  tmp[2,1] = "non command"
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "non command")
  
  tmp = control_file
  tmp = tmp[,c(ncol(tmp):1)]
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  suppressWarnings(expect_warning(validate_confidential_control_file(tmp, tbl), "Column 1 contains"))

})

test_that("Non-first columns match tbl", {
  
  tmp = control_file
  colnames(tmp)[3] = "made up name"
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "made up name")
  
})

test_that("At most one DROP, RENAME, ROUND, SEED, and TREAT_NA_AS", {
  
  cmd_vec = c("DROP", "RENAME", "ROUND", "TREAT_NA_AS", "SEED")
  for(cc in cmd_vec){
    
    tmp = control_file
    tmp[,1] = cc
    
    expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
    suppressWarnings(expect_warning(validate_confidential_control_file(tmp, tbl), cc))
  }
})

test_that("accepted rounding instructions", {
  
  tmp = control_file
  is_rounding = tmp[,1] == "ROUND"
  tmp[is_rounding,2:7] = c("rr3", "qq", "www", "conv10", "www", "qq")
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "'qq', 'www'")
})

test_that("missing NA options are numeric", {
  tmp = control_file
  is_na_handle = tmp[,1] == "MISSING_TO"
  tmp[is_na_handle,2:7] = c("one", "two", "three", "four", "five", "six")
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "'five', 'four', 'one'")
})

test_that("seed rules require column match", {
  tmp = control_file
  tmp[tmp == "seed"] = "not_present_column"

  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "not_present_column")
})

test_that("suppression rules match accepted pattern", {

  # non-suppression pattern match
  tmp = control_file
  tmp[9,9] = "not_a_suppression_pattern"
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "not_a_suppression_pattern")
  
  # suppression pattern match but not column match
  tmp = control_file
  tmp[9,9] = "made_up_column<100"
  
  expect_false(suppressWarnings(validate_confidential_control_file(tmp, tbl)))
  expect_warning(validate_confidential_control_file(tmp, tbl), "made_up_column")
  
})
