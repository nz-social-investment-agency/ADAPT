################################################################################
# Minimal script to execute the Assembly tool
# 
################################################################################

## User parameters -------------------------------------------------------- ----

# key inputs
control_file = "drive/folder/subfolder/control_file.xlsx"
sheet = "assembly"
assembly_table = "[database].[schema].[table]"
sql_folder = "drive/folder/subfolder/sql_folder"

## Database connection ---------------------------------------------------- ----

db_connection_string = "TO DO"

db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)

## Run assembly tool ------------------------------------------------------ ----

result_df = ADAPT::run_assembly(
  control_file = control_file,
  sheet = sheet,
  db_connection = db_connection,
  master_table = assembly_table
)

## All completed successfully --------------------------------------------- ----

DBI::dbDisconnect(db_connection)
no_assembly_failures = all(result_df$status == "Successful completion")
stopifnot(no_assembly_failures)
