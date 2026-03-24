################################################################################
# Example process to copy csv files to database
# 
# Useful for copying prepared examples to database for testing
################################################################################

## User parameters -------------------------------------------------------- ----

# key inputs
folder = "drive/folder/subfolder"

## Database connection ---------------------------------------------------- ----

# database connection - requires SQL Server in environment
db_connection_string = "TO DO"

db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)

## Helper functions-------------------------------------------------------- ----

drop_sql_table = function(db_connection, sql_table){
  drop_query = glue::glue("DROP TABLE IF EXISTS {sql_table}")
  DBI::dbExecute(db_connection, drop_query)
}

copy_r_to_sql = function(db_connection, sql_table, r_table) {
  suppressMessages( # mutes translation message
    DBI::dbWriteTable(
      db_connection,
      IDIr:::sql2id(sql_table),
      r_table
    )
  )
}

## Copy CSV to SQL -------------------------------------------------------- ----

# for each file
file_name = "file_name.csv"
sql_table = "[database].[schema].[table]"

local_data = read.csv(file.path(folder, file_name))
drop_sql_table(db_connection, sql_table)
copy_r_to_sql(db_connection, sql_table = sql_table, r_table = local_data)

## Conclude --------------------------------------------------------------- ----

DBI::dbDisconnect(db_connection)
