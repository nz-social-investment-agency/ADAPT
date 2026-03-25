################################################################################
# Minimal script to validate the Summary tool
# 
################################################################################

## User parameters -------------------------------------------------------- ----

# key inputs
control_file = "drive/folder/subfolder/control_file.xlsx"
sheet = "summary"
summary_table = "[database].[schema].[table]"
raw_summary_file = "drive/folder/subfolder/output - raw.csv"

## Database connection ---------------------------------------------------- ----

db_connection_string = "TO DO"

db_connection = DBI::dbConnect(odbc::odbc(), .connection_string = db_connection_string)
remote_master_table = dplyr::tbl(db_connection, ADAPT:::sql2id(summary_table))

## Validate summary tool -------------------------------------------------- ----

control_file = ADAPT::load_control_file(
  path_and_file_name = control_file,
  sheet = sheet
)

is_valid_summary_control_file = ADAPT::validate_summary_control_file(
  control_file = control_file,
  tbl = remote_master_table,
  save_file = raw_summary_file,
  partial_output = FALSE
)

## All completed successfully --------------------------------------------- ----

DBI::dbDisconnect(db_connection)
stopifnot(is_valid_summary_control_file)
