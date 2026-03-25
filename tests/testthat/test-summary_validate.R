# setup
control_file = system.file("extdata", "testing", "summary_tool", "control_file.csv", package = "ADAPT")
tbl = system.file("extdata", "testing", "summary_tool", "tbl.csv", package = "ADAPT")

# load
control_file = load_control_file(control_file)
tbl = read.csv(tbl)

# database connection - requires SQL Server in environment
db_connection_string = "NA"

can_connect = DBI::dbCanConnect(odbc::odbc(), .connection_string = db_connection_string)

if(nchar(db_connection_string) > 5 & !can_connect){
  stop("SQL Server connection string should not be part of package")
}

## test passing functionality --------------------------------------------- ----

test_that("worked example passes", {
  
  expect_true(validate_summary_control_file(control_file, tbl))
  expect_silent(validate_summary_control_file(control_file, tbl))
  
})

test_that("entity suffixes passes", {
  
  tmp_tbl = tbl
  tmp_tbl$alt_ent__min = 1
  tmp_tbl$alt_ent__max = 2
  
  tmp_cf = control_file
  tmp_cf$ENTITY2 = NA
  tmp_cf$ENTITY2[2] = "alt_ent"
  
  expect_true(validate_summary_control_file(tmp_cf, tmp_tbl))
  expect_silent(validate_summary_control_file(tmp_cf, tmp_tbl))
  
  # still pass with only one of min/max
  tmp_tbl$alt_ent__max = NULL
  
  expect_true(validate_summary_control_file(tmp_cf, tmp_tbl))
  expect_silent(validate_summary_control_file(tmp_cf, tmp_tbl))
  
  # fail with neither min.max
  tmp_tbl$alt_ent__min = NULL
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp_cf, tmp_tbl)))
  expect_warning(validate_summary_control_file(tmp_cf, tmp_tbl), "__min")
})

test_that("optional [] passes", {
  
  tmp_cf = control_file
  tmp_cf$GROUP1 = add_delimiters(control_file$GROUP1, "[]")
  
  expect_true(validate_summary_control_file(tmp_cf, tbl))
  expect_silent(validate_summary_control_file(tmp_cf, tbl))
})

## test failing functionality --------------------------------------------- ----

test_that("unaccepted columns warn", {
  
  tmp = control_file
  tmp$unusual_column_name = "c1"
  
  expect_true(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "unusual_column_name")
  
})

test_that("missing columns fail", {
  
  tmp = tbl
  tmp$uid = NULL
  
  expect_false(suppressWarnings(validate_summary_control_file(control_file, tmp)))
  suppressWarnings(expect_warning(validate_summary_control_file(control_file, tmp), "'uid' not found"))
  
  tmp = tbl
  tmp$age_group = NULL
  
  expect_false(suppressWarnings(validate_summary_control_file(control_file, tmp)))
  expect_warning(validate_summary_control_file(control_file, tmp), "age_group")
})

test_that("groups can not be dynamic", {
  
  tmp = control_file
  tmp$GROUP1 = "{ ifelse(is.na(region), 'x', region) }"
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "dynamic")
})

test_that("dynamic formula can fail", {
  
  tmp = control_file
  tmp$SUM[3] = "{2*island}"
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "Calculation .* errored")
  
})

test_that("empty columns warn", {
  
  tmp = tbl
  tmp$type = NA
  
  expect_true(suppressMessages(validate_summary_control_file(control_file, tmp)))
  expect_message(validate_summary_control_file(control_file, tmp), "missing values")
  
})

test_that("non-numeric sums warn", {
  
  tmp = control_file
  tmp$SUM[1] = "island"
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "sum non-numeric")
  
  tmp = tbl
  tmp$uid = as.character(tmp$uid)
  
  expect_false(suppressWarnings(validate_summary_control_file(control_file, tmp)))
  expect_warning(validate_summary_control_file(control_file, tmp), "sum non-numeric")
  
})

test_that("rows without summary fail", {
  
  tmp = control_file
  tmp[1,4:ncol(tmp)] = NA
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "row with no")
  
})

test_that("grouping and summarising with entity suffixes warns", {
  
  tmp_tbl = tbl
  tmp_tbl$alt_ent__min = 1
  tmp_tbl$alt_ent__max = 2
  
  tmp_cf = control_file
  tmp_cf$ENTITY2 = NA
  tmp_cf$ENTITY2[2] = "alt_ent"
  tmp_cf$DISTINCT[2] = tmp_cf$GROUP2[2]
  
  expect_true(suppressWarnings(validate_summary_control_file(tmp_cf, tmp_tbl)))
  expect_warning(validate_summary_control_file(tmp_cf, tmp_tbl), "double counting")
})

test_that("repeating column in grouping fails", {
  
  tmp = control_file
  tmp$GROUP2[1] = tmp$GROUP1[1]
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "used more than once for grouping")
})

test_that("enabled and disabled files fails", {
  
  tmp = control_file
  tmp$ENABLED[1] = FALSE
  
  expect_false(suppressWarnings(validate_summary_control_file(tmp, tbl)))
  expect_warning(validate_summary_control_file(tmp, tbl), "both enabled and disabled")
})

test_that("partial_output skips enabled and disabled files fails", {
  
  tmp = control_file
  tmp$ENABLED[1] = FALSE
  
  expect_true(validate_summary_control_file(tmp, tbl, partial_output = TRUE))
  expect_silent(validate_summary_control_file(tmp, tbl, partial_output = TRUE))
})


## testing SQL functionality ---------------------------------------------- ----

test_that("SQL checks passes",{
  skip_if_not(can_connect)
  db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)
  
  # copy table to SQL
  DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS IDI_Sandpit.[DL-MAA2023-46].tmp_tmp")
  
  DBI::dbWriteTable(
    db_connection,
    DBI::Id(catalog = "IDI_Sandpit", schema = "DL-MAA2023-46", table = "tmp_tmp"),
    tbl
  )
  tbl = dplyr::tbl(db_connection, from = I("IDI_Sandpit.[DL-MAA2023-46].tmp_tmp"))
  
  # test
  expect_true(validate_summary_control_file(control_file, tbl))
  
  control_file$WHERE[1] = "{ region %in% c(1,2) }"
  expect_true(validate_summary_control_file(control_file, tbl))
  
  # tidy up to conclude
  DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS IDI_Sandpit.[DL-MAA2023-46].tmp_tmp")
  DBI::dbDisconnect(db_connection)
})
