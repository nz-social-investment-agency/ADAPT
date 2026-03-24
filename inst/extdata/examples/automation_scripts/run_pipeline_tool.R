################################################################################
# Minimal script to execute the Pipeline tool
# 
################################################################################

## User parameters -------------------------------------------------------- ----

# key inputs
control_file = "drive/folder/subfolder/pipeline_control_file.xlsx"
sheet = "pipeline"

# delay
delay_minutes = 60

# write console log to file
sink_file = "drive/folder/subfolder/run_pipeline_log.txt"
# pipeline will append to an existing log, delete existing log to start over

## Database connection ---------------------------------------------------- ----

db_connection_string = "TO DO"

## Ensure required packages are available --------------------------------- ----

stopifnot("openxlsx2" %in% installed.packages())
stopifnot("IDIr" %in% installed.packages())

## Run pipeline tool ------------------------------------------------------ ----

IDIr::run_pipeline(
  control_file = control_file,
  sheet = sheet,
  db_connection_string = db_connection_string,
  delay_minutes = delay_minutes,
  sink_file = sink_file
)

