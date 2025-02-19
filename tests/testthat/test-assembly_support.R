################################################################################
#' Notes
#' 
################################################################################

## alter_table_drop_column(table_name, columns, if_exists = TRUE) --------- ----

test_that("single column dropped", {
  
  actual = alter_table_drop_column("mt", "col_name")
  
  expect_true(grepl("ALTER TABLE mt", actual))
  expect_true(grepl("DROP\\nCOLUMN ", actual))
  expect_true(grepl("COLUMN IF EXISTS \\[col_name\\]", actual))
})

test_that("multi column dropped", {
  
  actual = alter_table_drop_column("mt", c("col1", "col2"))
  
  expect_true(grepl("ALTER TABLE mt", actual))
  expect_true(grepl("DROP\\nCOLUMN ", actual))
  expect_true(grepl("COLUMN IF EXISTS \\[col1\\]", actual))
  expect_true(grepl("COLUMN IF EXISTS \\[col2\\]", actual))
})

test_that("existance turns on and off", {
  
  actual = alter_table_drop_column("mt", "col_name", FALSE)
  
  expect_true(grepl("COLUMN \\[col_name\\]", actual))
})

## alter_table_add_column(table_name, columns, types) --------------------- ----

test_that("single column added", {
  
  actual = alter_table_add_column("mt", "col_name", "int")
  
  expect_true(grepl("ALTER TABLE mt ADD", actual))
  expect_true(grepl("\\[col_name\\] INT", actual))
})

test_that("multi column added", {
  actual = alter_table_add_column("mt", c("col1", "col2"), c("varchar(10)", "date"))
  
  expect_true(grepl("ALTER TABLE mt ADD", actual))
  expect_true(grepl("\\[col1\\] VARCHAR\\(10\\)", actual))
  expect_true(grepl("\\[col2\\] DATE", actual))
})

test_that("invalid type errors", {
  
  expect_error(alter_table_add_column("mt", "col", "iint"))
  expect_error(alter_table_add_column("mt", "col", "deta"))
  
})

## is_valid_data_type(type) ----------------------------------------------- ----

test_that("accepted types return true", {
  
  test_types = c("INT", "date", "VARCHAR(10)", "numeric(6,3)")
  
  for(tt in test_types){
    expect_true(is_valid_data_type(tt))
  }
})

test_that("unaccepted types return false", {
  
  test_types = c("IINT", "data", "VARCHAR10", "numerc")
  
  for(tt in test_types){
    expect_false(is_valid_data_type(tt))
  }
})

## sql_file_exists_and_contains(file, text) ------------------------------- ----

test_that("found file and text return true", {
  
  tmp_file = file.path(tempdir(), "testing.sql")
  writeLines("example\n text here\nand here", tmp_file)
  
  expect_true(sql_file_exists_and_contains(tmp_file, "text"))
  
})

test_that("found file without text returns false", {
  
  tmp_file = file.path(tempdir(), "testing2.sql")
  writeLines("example\n text here\nand here", tmp_file)
  
  expect_false(sql_file_exists_and_contains(tmp_file, "makeup-text"))
})

test_that("unfound file returns false", {
  tmp_file = file.path(tempdir(), "testing3.sql")
  expect_false(sql_file_exists_and_contains(tmp_file, "text"))
})

test_that("text only in comments returns false", {
  tmp_file = file.path(tempdir(), "testing4.sql")
  writeLines("/*\n comment1 text\n*/\nmore text\nenven more text -- comment2 \nend of text", tmp_file)
  
  expect_true(sql_file_exists_and_contains(tmp_file, "text"))
  expect_false(sql_file_exists_and_contains(tmp_file, "comment1"))
  expect_false(sql_file_exists_and_contains(tmp_file, "comment2"))
})

test_that("special characters handled", {
  tmp_file = file.path(tempdir(), "testing5.sql")
  writeLines("examples . here\n a + b \n (foo | bar) \n", tmp_file)
  
  expect_true(sql_file_exists_and_contains(tmp_file, "."))
  expect_false(sql_file_exists_and_contains(tmp_file, ".."))
  
  expect_true(sql_file_exists_and_contains(tmp_file, "a + b"))
  expect_true(sql_file_exists_and_contains(tmp_file, "(foo"))
  
})

test_that("square brackets are optional", {
  tmp_file = file.path(tempdir(), "testing6.sql")
  writeLines("SELECT [db].schema.[table] \n", tmp_file)
  
  expect_true(sql_file_exists_and_contains(tmp_file, "[db].[schema].[table]"))
})

test_that("multiple text strings found at once", {
  tmp_file = file.path(tempdir(), "testing7.sql")
  writeLines(" a b c [d] e \n", tmp_file)
  
  actual = sql_file_exists_and_contains(tmp_file, c("a","b","[c]","[d]","f"))
  expected = c(TRUE, TRUE, TRUE, TRUE, FALSE)
  expect_equal(actual, expected)
})

test_that("comments don't interfere with non-connets", {
  tmp_file = file.path(tempdir(), "testing8.sql")
  writeLines("a--b\nc/*d*/e\nf\n/*\ng\n*/h", tmp_file)
  
  actual = sql_file_exists_and_contains(tmp_file, c("a", "b", "c", "d", "e","f", "g", "h"))
  expected = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  expect_equal(actual, expected)
})

test_that("test files found", {
  
  # testing folder
  test_folder = system.file("extdata", "testing", "assembly_tool", package = "IDIr")
  
  expect_true(sql_file_exists_and_contains(file.path(test_folder, "demo_script_accidents.sql"), "tmp_accidents"))
  
  expect_true(sql_file_exists_and_contains(file.path(test_folder, "demo_script_benefits.sql"), "tmp_benefit_payment"))
})

