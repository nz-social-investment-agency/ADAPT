################################################################################
# Notes:
# - Creates SQLite database for testing assembly tool
# - Run to setup basic database for package testing
#
################################################################################

## setup ------------------------------------------------------------------ ----

required_packages = c("DBI", "RSQLite")
stopifnot(all(required_packages %in% installed.packages()))

path = system.file("extdata", "testing", "assembly_tool", package = "IDIr")
db_path = file.path(path, "testing_sqlite.db")

# delete old database
unlink(db_path)
db_conn = DBI::dbConnect(RSQLite::SQLite(), db_path)

## write to database supporting function ---------------------------------- ----

copy_r_to_sql = function(db_connection, sql_table, r_table) {
  stopifnot("tbl_sql" %not_in% class(r_table))
  
  # copy data
  suppressMessages( # mutes any translation message
    DBI::dbWriteTable(db_connection, DBI::Id(table = sql_table), r_table)
  )
}

## load data csv's to db -------------------------------------------------- ----

data_accidents = read.csv(file.path(path, "data_accidents.csv"), stringsAsFactors = FALSE)
data_benefit_payment = read.csv(file.path(path, "data_benefit_payment.csv"), stringsAsFactors = FALSE)
data_master_table = read.csv(file.path(path, "data_master_table.csv"), stringsAsFactors = FALSE)

copy_r_to_sql(db_conn, sql_table = "tmp_accidents", r_table = data_accidents)
copy_r_to_sql(db_conn, sql_table = "tmp_benefit_payment", r_table = data_benefit_payment)
copy_r_to_sql(db_conn, sql_table = "tmp_master_table", r_table = data_master_table)

## confirm contents ------------------------------------------------------- ----

print(DBI::dbListTables(db_conn))
# For SQLite - store dates as text in YYYY-MM-DD format
print(dplyr::collect(dplyr::tbl(db_conn, "tmp_accidents")))
print(dplyr::collect(dplyr::tbl(db_conn, "tmp_benefit_payment")))
print(dplyr::collect(dplyr::tbl(db_conn, "tmp_master_table")))

## Close connection ------------------------------------------------------- ----

DBI::dbDisconnect(db_conn)
