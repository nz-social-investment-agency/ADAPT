################################################################################
# Confidentialise files
# 
################################################################################

## User parameters -------------------------------------------------------- ----

# key inputs
control_file = "drive/folder/subfolder/control_file.xlsx"
sheet = "confidentialise"
raw_summary_file = "drive/folder/subfolder/output - raw.csv"
conf_summary_file = "drive/folder/subfolder/output - conf.csv"

## Confidentialise -------------------------------------------------------- ----

# read
tbl = read.csv(raw_summary_file)
# round
tbl = IDIr::run_confidential(control_file, sheet, tbl)
# write
write.csv(tbl, conf_summary_file, row.names = FALSE)
