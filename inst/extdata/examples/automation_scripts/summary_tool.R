################################################################################
# Minimal script to execute the Summary tool
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
remote_master_table = dplyr::tbl(db_connection, IDIr:::sql2id(summary_table))

## Run summary tool ------------------------------------------------------- ----

result_df = IDIr::run_summary(
  control_file = control_file,
  sheet = summary_sheet,
  tbl = remote_master_table,
  save_file = raw_summary_file
)

## All completed successfully --------------------------------------------- ----

DBI::dbDisconnect(db_connection)
no_summary_failures = all(result_df$status == "Successful completion")
stopifnot(no_summary_failures)
