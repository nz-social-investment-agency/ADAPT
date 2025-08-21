# setup
control_file = system.file("extdata", "testing", "confidential_tool", "control_file.csv", package = "IDIr")
tbl = system.file("extdata", "testing", "confidential_tool", "tbl.csv", package = "IDIr")
results = system.file("extdata", "testing", "confidential_tool", "results.csv", package = "IDIr")

# load
tbl = read.csv(tbl)
results = read.csv(results)

# sort
results = dplyr::tibble(results)
results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))

## helper for checking rounding ------------------------------------------- ----

help_check_rounding = function(cc, control_file, actual, results){
  
  control_file = load_control_file(control_file)
  
  # get rounding type
  renaming = control_file[,1] == "RENAME"
  renamed_position = which(control_file[renaming,] == gsub("conf_", "", cc))
  original_position = which(colnames(control_file) == gsub("conf_", "", cc))
  
  rounding = control_file[,1] == "ROUND"
  rounding_type = control_file[rounding, c(renamed_position, original_position)]
  
  # exit early if no rounding type
  if(is.na(rounding_type)){
    return(NULL)
  }
  
  # difference
  difference = abs(dplyr::coalesce(actual[[cc]], -1) - dplyr::coalesce(results[[cc]], -1))
  
  # check rounding (1) some rounding (2) of the right magnitude
  if(rounding_type == "RR3"){
    expect_true(all(difference == 3 | difference == 0))
  }
  if(rounding_type == "CONV10"){
    expect_true(all(difference == 0))
  }
  if(rounding_type == "CONV100"){
    expect_true(all(difference == 0))
  }
  if(rounding_type == "CONV1000"){
    expect_true(all(difference == 0))
  }
  if(rounding_type == "GRR"){
    expect_true(all(difference == 0 | difference == 3 | difference %% 5 == 0 | actual[[cc]] == 19))
  }
  
}

## test functionality ----------------------------------------------------- ----

test_that("worked example passes",{
  
  actual = run_confidential(control_file, sheet = NULL, tbl)
  actual = dplyr::tibble(actual)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  # loop through columns
  for(cc in colnames(results)){
    if(substr(cc, 1, 5) != "conf_"){
      expect_equal(actual[[cc]], results[[cc]])
      next
    }
    
    # rounding
    help_check_rounding(cc, control_file, actual, results)
    
    # suppression
    expect_true(all(is.na(actual[[cc]]) == is.na(results[[cc]])))
  }
})

test_that("NA handling effects output",{
  
  # setup
  actual_unmodified = run_confidential(control_file, sheet = NULL, tbl)
  
  tmp = load_control_file(control_file, sheet = NULL)
  tmp$entity1[tmp$COMMAND == "MISSING_TO"] = 0
  
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(tmp, tmp_cf, row.names = FALSE)
  
  actual_modified = run_confidential(tmp_cf, sheet = NULL, tbl)
  
  # standardise
  actual_unmodified = dplyr::select(actual_unmodified, dplyr::all_of(colnames(results)))
  actual_modified = dplyr::arrange(actual_modified, !!!rlang::syms(colnames(results)))
  
  # structure consistent
  expect_equal(nrow(actual_unmodified), nrow(results))
  expect_equal(ncol(actual_unmodified), ncol(results))
  expect_equal(nrow(actual_modified), nrow(results))
  expect_equal(ncol(actual_modified), ncol(results))
  
  expect_equal(colnames(actual_unmodified), colnames(actual_modified))
  
  # contents changed
  more_suppression_in_modified = sum(is.na(actual_modified)) > sum(is.na(actual_unmodified))
  expect_true(more_suppression_in_modified > 0)
  
})

test_that("thresholds respected",{
  # all 19's
  tmp = tbl
  tmp$distinct1 = 19
  
  actual = run_confidential(control_file, sheet = NULL, tmp, stable_above = 1000)
  
  # RR3 to 18
  non_nas = !is.na(actual$conf_num_hhlds)
  expect_true(sum(non_nas) > 0)
  expect_true(all(actual$conf_num_hhlds[non_nas] == 18))
  
  # all 20's
  tmp = tbl
  tmp$distinct1 = 20
  
  actual = run_confidential(control_file, sheet = NULL, tmp, stable_above = 1000)
  
  # RR3 to 21
  non_nas = !is.na(actual$conf_num_hhlds)
  expect_true(sum(non_nas) > 0)
  expect_true(all(actual$conf_num_hhlds[non_nas] == 21))
  
  # all 18's
  tmp = tbl
  tmp$count1 = 19
  
  actual = run_confidential(control_file, sheet = NULL, tmp, stable_above = 1000)
  
  # GRR to 18
  non_nas = !is.na(actual$conf_num_people)
  expect_true(sum(non_nas) > 0)
  expect_true(all(actual$conf_num_people[non_nas] == 18))
  
  # all 44's
  tmp = tbl
  tmp$count1 = 44
  
  cf = load_control_file(control_file, sheet = NULL)
  cf[6,2] = "count1 < 42"
  
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(cf, tmp_cf, row.names = FALSE)
  
  actual = run_confidential(tmp_cf, sheet = NULL, tmp, stable_above = 1000)
  
  # GRR to 45
  non_nas = !is.na(actual$conf_num_people)
  expect_true(sum(non_nas, na.rm = TRUE) > 0)
  expect_true(all(actual$conf_num_people[non_nas] == 45))
  
})

test_that("consistent rounding in RR3",{
  
  ## reading seeds from control file
  tmp = tbl
  tmp$distinct1 = 50
  tmp$seed = 123
  
  # single value
  actual = run_confidential(control_file, sheet = NULL, tmp)
  non_nas = !is.na(actual$conf_num_hhlds)
  expect_true(sum(non_nas) > 0)
  expect_true(length(unique(actual$conf_num_hhlds[non_nas])) == 1)
  
  ## generating seeds
  tmp = tbl
  tmp$distinct1 = 50
  
  cf = load_control_file(control_file, sheet = NULL)
  cf[5,9:10] = NA
  
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(cf, tmp_cf, row.names = FALSE)
  
  # single value
  actual = run_confidential(tmp_cf, sheet = NULL, tmp)
  non_nas = !is.na(actual$conf_num_hhlds)
  expect_true(sum(non_nas) > 0)
  expect_true(length(unique(actual$conf_num_hhlds[non_nas])) == 1)
  
  ## deactivating via `stable_above`
  
  # multi-values value
  actual = run_confidential(tmp_cf, sheet = NULL, tmp, stable_above = Inf)
  non_nas = !is.na(actual$conf_num_hhlds)
  expect_true(sum(non_nas) > 0)
  expect_true(length(unique(actual$conf_num_hhlds[non_nas])) > 1)
})

test_that("consistent rounding in GRR",{
  
  ## reading seeds from control file
  tmp = tbl
  tmp$count1 = 905
  tmp$seed = 789
  
  # single value
  actual = run_confidential(control_file, sheet = NULL, tmp)
  non_nas = !is.na(actual$conf_num_people)
  expect_true(sum(non_nas) > 0)
  expect_true(length(unique(actual$conf_num_people[non_nas])) == 1)
  
  ## generating seeds
  tmp = tbl
  tmp$count1 = 905
  
  cf = load_control_file(control_file, sheet = NULL)
  cf[5,9:10] = NA
  
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(cf, tmp_cf, row.names = FALSE)
  
  # single value
  actual = run_confidential(tmp_cf, sheet = NULL, tmp)
  non_nas = !is.na(actual$conf_num_people)
  expect_true(sum(non_nas) > 0)
  expect_true(length(unique(actual$conf_num_people[non_nas])) == 1)
  
  ## deactivating via `stable_above`
  
  # multi-values value
  actual = run_confidential(tmp_cf, sheet = NULL, tmp, stable_above = Inf)
  non_nas = !is.na(actual$conf_num_people)
  expect_true(sum(non_nas) > 0)
  expect_true(length(unique(actual$conf_num_people[non_nas])) > 1)
})

test_that("errors in control file prevent execution", {
  
  tmp = load_control_file(control_file, sheet = NULL)
  colnames(tmp)[4] = "non_existant_column"
  
  tmp_cf = file.path(tempdir(), "tmp_cf.csv")
  write.csv(tmp, tmp_cf, row.names = FALSE)
  
  expect_error(suppressWarnings(run_confidential(tmp_cf, sheet = NULL, tbl)))
})

## test examples ---------------------------------------------------------- ----

test_that("confidential_simple_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "confidential_simple_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "confidential_simple_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "confidential_simple_worked_example", "results.csv", package = "IDIr")
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  actual = run_confidential(control_file, sheet = NULL, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  # loop through columns
  for(cc in colnames(results)){
    if(substr(cc, 1, 5) != "conf_"){
      expect_equal(actual[[cc]], results[[cc]])
      next
    }
    
    # rounding
    help_check_rounding(cc, control_file, actual, results)
    
    # suppression
    expect_true(all(is.na(actual[[cc]]) == is.na(results[[cc]])))
  }
})

test_that("confidential_survey_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "confidential_survey_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "confidential_survey_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "confidential_survey_worked_example", "results.csv", package = "IDIr")
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  actual = run_confidential(control_file, sheet = NULL, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  # loop through columns
  for(cc in colnames(results)){
    if(substr(cc, 1, 5) != "conf_"){
      expect_equal(actual[[cc]], results[[cc]])
      next
    }
    
    # rounding
    help_check_rounding(cc, control_file, actual, results)
    
    # suppression
    expect_true(all(is.na(actual[[cc]]) == is.na(results[[cc]])))
  }
})

test_that("confidential_wide_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "confidential_wide_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "confidential_wide_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "confidential_wide_worked_example", "results.csv", package = "IDIr")
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  actual = run_confidential(control_file, sheet = NULL, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  # loop through columns
  for(cc in colnames(results)){
    if(substr(cc, 1, 5) != "conf_"){
      expect_equal(actual[[cc]], results[[cc]])
      next
    }
    
    # rounding
    help_check_rounding(cc, control_file, actual, results)
    
    # suppression
    expect_true(all(is.na(actual[[cc]]) == is.na(results[[cc]])))
  }
})

test_that("confidential_stable_worked_example passes", {
  
  # setup
  control_file = system.file("extdata", "examples", "confidential_stable_worked_example", "control_file.csv", package = "IDIr")
  tbl = system.file("extdata", "examples", "confidential_stable_worked_example", "tbl.csv", package = "IDIr")
  results = system.file("extdata", "examples", "confidential_stable_worked_example", "results.csv", package = "IDIr")
  
  # load
  tbl = read.csv(tbl)
  results = read.csv(results)
  
  # sort
  results = dplyr::tibble(results)
  results = dplyr::arrange(results, !!!rlang::syms(colnames(results)))
  
  actual = run_confidential(control_file, sheet = NULL, tbl)
  
  expect_equal(nrow(actual), nrow(results))
  expect_equal(ncol(actual), ncol(results))
  
  expect_equal(colnames(actual), colnames(results))
  
  actual = dplyr::select(actual, dplyr::all_of(colnames(results)))
  actual = dplyr::arrange(actual, !!!rlang::syms(colnames(results)))
  
  # loop through columns
  for(cc in colnames(results)){
    if(substr(cc, 1, 5) != "conf_"){
      expect_equal(actual[[cc]], results[[cc]])
      next
    }
    # rounding
    help_check_rounding(cc, control_file, actual, results)
  }
  
  # consistent rounding by seed - conf_group_size
  tmp = dplyr::group_by(actual, group_size)
  tmp = dplyr::summarise(tmp, dist = dplyr::n_distinct(conf_group_size), .groups = "drop")
  expect_true(all(tmp$dist == 1))
  
  # inconsistent rounding without seed - conf_value
  tmp = dplyr::group_by(actual, value)
  tmp = dplyr::summarise(tmp, dist = dplyr::n_distinct(conf_value), .groups = "drop")
  expect_false(all(tmp$dist == 1))
})
