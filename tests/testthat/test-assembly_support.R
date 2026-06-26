## alter_table_drop_column(table_name, columns, if_exists = TRUE) --------- ----

test_that("single column dropped", {
  
  actual = alter_table_drop_column("mt", "col_name")
  
  expect_true(grepl("ALTER TABLE \\[mt\\]", actual))
  expect_true(grepl("DROP COLUMN ", actual))
  expect_true(grepl("COLUMN IF EXISTS \\[col_name\\]", actual))
})

test_that("multi column dropped", {
  
  actual = alter_table_drop_column("mt", c("col1", "col2"))
  
  expect_true(grepl("ALTER TABLE \\[mt\\]", actual))
  expect_true(grepl("DROP COLUMN ", actual))
  expect_true(grepl("COLUMN IF EXISTS \\[col1\\]", actual))
  expect_true(grepl("COLUMN IF EXISTS \\[col2\\]", actual))
})

test_that("existance turns on and off", {
  
  actual = alter_table_drop_column("mt", "col_name", if_exists = FALSE)
  
  expect_true(grepl("COLUMN \\[col_name\\]", actual))
})

## alter_table_add_column(table_name, columns, types) --------------------- ----

test_that("single column added", {
  
  actual = alter_table_add_column("mt", "col_name", "int")
  
  expect_true(grepl("ALTER TABLE \\[mt\\] ADD", actual))
  expect_true(grepl("\\[col_name\\] INT", actual))
})

test_that("multi column added", {
  actual = alter_table_add_column("mt", c("col1", "col2"), c("varchar(10)", "date"))
  
  expect_true(grepl("ALTER TABLE \\[mt\\] ADD", actual))
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
  test_folder = system.file("extdata", "testing", "assembly_tool", package = "ADAPT")
  
  expect_true(sql_file_exists_and_contains(file.path(test_folder, "demo_script_accidents.sql"), "tmp_accidents"))
  
  expect_true(sql_file_exists_and_contains(file.path(test_folder, "demo_script_benefits.sql"), "tmp_benefit_payment"))
})

## entity_to_min_and_max(control_file) ------------------------------------ ----

test_that("no entity rows returned unchanged", {
  control_file = data.frame(
    measure_value = c("a", "b"),
    output_name = c("x", "y"),
    output_method = c("COUNT", "SUM"),
    output_type = c("INT", "FLOAT"),
    stringsAsFactors = FALSE
  )
  
  actual = entity_to_min_and_max(control_file)
  
  expect_equal(actual, control_file)
})

test_that("entity rows duplicated", {
  control_file = data.frame(
    measure_value = c("a", "b", "c"),
    output_name = c("x", "y", "z"),
    output_method = c("COUNT", "ENTITY", "SUM"),
    output_type = c("INT", "VARCHAR(50)", "FLOAT"),
    stringsAsFactors = FALSE
  )
  
  actual = entity_to_min_and_max(control_file)
  
  expected = data.frame(
    measure_value = c("a", "b", "b", "c"),
    output_name = c("x", "y__min", "y__max", "z"),
    output_method = c("COUNT", "MIN", "MAX", "SUM"),
    output_type = c("INT", "VARCHAR(50)", "VARCHAR(50)", "FLOAT"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("multiple entity rows all duplicate", {
  control_file = data.frame(
    measure_value = c("a", "b", "c", "d", "e"),
    output_name = c("v", "\"w\"", "x", "y", "z"),
    output_method = c("COUNT", "ENTITY", "SUM", "ENTITY", "MAX"),
    output_type = c("INT", "VARCHAR(50)", "FLOAT", "INT", "INT"),
    stringsAsFactors = FALSE
  )
  
  actual = entity_to_min_and_max(control_file)
  
  expected = data.frame(
    measure_value = c("a", "b", "b", "c", "d", "d", "e"),
    output_name = c("v", "\"w__min\"", "\"w__max\"", "x", "y__min", "y__max", "z"),
    output_method = c("COUNT", "MIN", "MAX", "SUM", "MIN", "MAX", "MAX"),
    output_type = c("INT", "VARCHAR(50)", "VARCHAR(50)", "FLOAT", "INT", "INT", "INT"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

## handle_summary_case(control_file_row, sqlite) -------------------------- ----

test_that("SQL Server MIN types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "MIN",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "MIN(mv) AS name")
})

test_that("SQL Server MAX types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "MAX",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "MAX(mv) AS name")
})

test_that("SQL Server EXISTS types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "EXISTS",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "IIF(COUNT(mv) >= 1, 1, NULL) AS name")
})

test_that("SQL Server COUNT types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "COUNT",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "COUNT(mv) AS name")
})

test_that("SQL Server MEAN types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "MEAN",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "AVG(mv) AS name")
})

test_that("SQL Server DISTINCT types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "DISTINCT",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "COUNT(DISTINCT mv) AS name")
})

test_that("SQL Server ENTITY types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "ENTITY",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, c("MIN(mv) AS name__min", "MAX(mv) AS name__max"))
})

test_that("SQL Server SUM types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expect_equal(actual, "SUM(mv) AS name")
})

test_that("SQL Server SUM_WITHIN types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "SUM_WITHIN",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expected = paste0(
    "SUM(1.0 * ",
    "(1+DATEDIFF(DAY, IIF(ms < dmt.core_query_p_start, dmt.core_query_p_start, ms), IIF(me < dmt.core_query_p_end, me, dmt.core_query_p_end)))",
    "/ (1+DATEDIFF(DAY, ms, me)) * mv) AS name"
  )
  
  actual = as.character(gsub("[[:space:]]", "", actual))
  expected = gsub("[[:space:]]", "", expected)
  
  expect_equal(actual, expected)
})

test_that("SQL Server DURATION types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "DURATION",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row)
  
  expected = "SUM(IIF(mv IS NULL, NULL, 1+DATEDIFF(DAY, IIF(ms < dmt.core_query_p_start, dmt.core_query_p_start, ms), IIF(me < dmt.core_query_p_end, me, dmt.core_query_p_end)))) AS name"

  actual = as.character(gsub("[[:space:]]", "", actual))
  expected = gsub("[[:space:]]", "", expected)
  
  expect_equal(actual, expected)
})

test_that("SQLite SUM_WITHIN types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "SUM_WITHIN",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row, sqlite = TRUE)
  
  expected = paste0(
    "SUM(1.0 * ",
    "(1 + JULIANDAY(IIF(me < dmt.core_query_p_end, me, dmt.core_query_p_end)) - JULIANDAY(IIF(ms < dmt.core_query_p_start, dmt.core_query_p_start, ms)))",
    "/ (1 + JULIANDAY(me) - JULIANDAY(ms)) * mv) AS name"
  )
  
  actual = as.character(gsub("[[:space:]]", "", actual))
  expected = gsub("[[:space:]]", "", expected)
  
  expect_equal(actual, expected)
})
  
test_that("SQLite DURATION types handle correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "mv",
    output_name = "name",
    output_method = "DURATION",
    stringsAsFactors = FALSE
  )
  
  actual = handle_summary_case(row, sqlite = TRUE)
  
  expected = "SUM(IIF(mv IS NULL, NULL, 1 + JULIANDAY(IIF(me < dmt.core_query_p_end, me, dmt.core_query_p_end)) - JULIANDAY(IIF(ms < dmt.core_query_p_start, dmt.core_query_p_start, ms)))) AS name"
  
  actual = as.character(gsub("[[:space:]]", "", actual))
  expected = gsub("[[:space:]]", "", expected)
  
  expect_equal(actual, expected)
})

test_that("constants handled correctly", {
  row = data.frame(
    period_start = "[ps]",
    period_end = "[pe]",
    measure_start = "[ms]",
    measure_end = "[me]",
    measure_value = "\"1\"",
    output_name = "\"name\"",
    output_method = "MIN",
    stringsAsFactors = FALSE
  )
  
  row = handle_delimiters_and_prefixes(row, "mt", c("ps", "pe", "puid"), "m", c("ms", "me", "mv", "muid"))
  actual = handle_summary_case(row)
  
  expect_equal(actual, "MIN('1') AS [name]")
})

test_that("dynamics handled correctly", {
  row = data.frame(
    period_start = "ps",
    period_end = "pe",
    measure_start = "ms",
    measure_end = "me",
    measure_value = "IIF(mv > 0, mv, NULL)",
    output_name = "\"name\"",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  row = handle_delimiters_and_prefixes(row, "mt", c("ps", "pe", "puid"), "m", c("ms", "me", "mv", "muid"))
  actual = handle_summary_case(row)
  
  expect_equal(actual, "SUM(IIF(m.mv > 0, m.mv, NULL)) AS [name]")
})

## handle_delimiters_and_prefixes(df, mt_prefix, mt_cols, measure_prefix, measure_cols) ----

test_that("delimiters changed as expected", {
  row = data.frame(
    period_start = "{ps}",
    period_end = "\"pe\"",
    measure_start = "\"ms\"",
    measure_end = "{me}",
    measure_value = "[mv]",
    output_name = "\"name\"",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  actual = handle_delimiters_and_prefixes(row, "x", "non_existance_col", "z", "non_existance_col")
  
  expected = data.frame(
    period_start = "ps",
    period_end = "'pe'",
    measure_start = "'ms'",
    measure_end = "me",
    measure_value = "[mv]",
    output_name = "[name]",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("master table prefixes added as expected", {
  row = data.frame(
    period_start = "{ IIF(ps > 0, 1, NULL) }",
    period_end = "[pe]",
    measure_start = "[ms]",
    measure_end = "{me}",
    measure_value = "[mv]",
    output_name = "\"name\"",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  actual = handle_delimiters_and_prefixes(row, "x", c("ps", "pe"), "z", "non_existance_col")
  
  expected = data.frame(
    period_start = "IIF(x.ps > 0, 1, NULL)",
    period_end = "x.[pe]",
    measure_start = "[ms]",
    measure_end = "me",
    measure_value = "[mv]",
    output_name = "[name]",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})

test_that("measure table prefixes added as expected", {
  row = data.frame(
    period_start = "{ IIF(ps > 0, 1, NULL) }",
    period_end = "[pe]",
    measure_start = "{\"ms\"}",
    measure_end = "{me}",
    measure_value = "{ DATEDIFF(DAY, me, [ms]) }",
    output_name = "\"name\"",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  actual = handle_delimiters_and_prefixes(row, "x", "non_existance_col", "z", c("me","ms", "mv","muid"))
  
  expected = data.frame(
    period_start = "IIF(ps > 0, 1, NULL)",
    period_end = "[pe]",
    measure_start = "z.\"ms\"",
    measure_end = "z.me",
    measure_value = "DATEDIFF(DAY, z.me, z.[ms])",
    output_name = "[name]",
    output_method = "SUM",
    stringsAsFactors = FALSE
  )
  
  expect_equal(actual, expected)
})
