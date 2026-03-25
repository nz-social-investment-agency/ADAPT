################################################################################
# Minimal script to validate the Assembly tool
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

## Validate assembly tool ------------------------------------------------- ----

control_file = ADAPT::load_control_file(
  path_and_file_name = control_file,
  sheet = sheet
)

is_valid_assembly_control_file = ADAPT::validate_assembly_control_file(
  control_file = control_file,
  db_connection = db_connection,
  master_table = assembly_table
)

## All completed successfully --------------------------------------------- ----

DBI::dbDisconnect(db_connection)
stopifnot(is_valid_assembly_control_file)
