SELECT snz_uid
	, accident_date
	, accident_cause
	, recovery_time
	, rehab_uid
INTO [IDI_Sandpit].[DL-MAA20XX-YY].[tmp_accidents]
FROM raw_source_data


-- for testing file contents check
SELECT column_within_file
FROM table_within_file
