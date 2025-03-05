/*
Documentation header
2025-02-26
*/

SELECT TOP 3 snz_uid
FROM [IDI_Clean_202410].[data].[personal_detail]
GO

-- Should not be necessary as start from new connection
-- with no existing temporary tables
-- DROP TABLE IF EXISTS #temp


CREATE TABLE #temp (
	col1 INT
	, col2 VARCHAR(5)
	, col3 DATE
);

INSERT INTO #temp
VALUES
(   1, 'aaa', '2020-01-01'),
(NULL, 'bbb', '2021-01-01'),
(   3,  NULL, '2022-01-01'),
(   4, 'ddd',         NULL),
(   5, 'aaa', '2020-01-01'),
(NULL, 'bbb', '2021-01-01'),
(   7, 'ccc', '2022-01-01'),
(   8, 'ddd',         NULL),
(   9, 'aaa', '2020-01-01'),
(NULL, 'bbb', '2021-01-01'),
(  11,  NULL, '2022-01-01'),
(  12, 'ddd', '2023-01-01')
