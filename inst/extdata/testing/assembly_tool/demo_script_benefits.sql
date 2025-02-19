SELECT snz_uid
	, benefit_start_date
	, benefit_end_date
	, payments
INTO [IDI_Sandpit].[DL-MAA20XX-YY].[tmp_benefit_payment]
FROM raw_source_data

-- for testing file contents check
SELECT column_within_file
FROM table_within_file
