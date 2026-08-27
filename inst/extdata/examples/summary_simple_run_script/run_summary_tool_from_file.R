################################################################################
# Run ADAPT summary tool against local data file
# 
################################################################################

## User parameters -------------------------------------------------------- ----

# Control file (.xlsx or .csv)
control_file = "drive/folder/subfolder/control_file.xlsx"
control_file_sheet = "summary" # ignored if control file is a CSV

# Input data file (.xlsx or .csv)
input_data_file = "drive/folder/subfolder/input_data.csv"
input_sheet = "Sheet1" # ignored if input data file is a CSV

# Output CSV file
output_summary_file = "drive/folder/subfolder/summarised_output.csv"

## Package management ----------------------------------------------------- ----

# ADAPT must be installed
stopifnot("ADAPT" %in% installed.packages())

# Enforce required extensions
stopifnot(tools::file_ext(control_file) %in% c("xlsx", "csv"))
stopifnot(tools::file_ext(input_data_file) %in% c("xlsx", "csv"))
stopifnot(tools::file_ext(output_summary_file) == "csv")

## Read in data file ------------------------------------------------------ ----

if(tools::file_ext(input_data_file) == "xlsx"){
  tbl = openxlsx2::read_xlsx(input_data_file, input_sheet)
}
if(tools::file_ext(input_data_file) == "csv"){
  tbl = read.csv(input_data_file)
}

## Run summary tool ------------------------------------------------------- ----

result_df = ADAPT::run_summary(
  control_file = control_file,
  sheet = control_file_sheet,
  tbl = tbl,
  save_file = output_summary_file
)
