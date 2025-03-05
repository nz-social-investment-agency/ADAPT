-- Documentation header
-- 2025-02-26

-- This file will error when run from the pipeline
-- as it depends on #temp which is not created
-- in this file.

SELECT *
FROM #temp;


WITH step1 AS (
	SELECT *
	FROM #temp
	WHERE col3 IS NOT NULL
)
SELECT SUM(col1) AS total_value
FROM step1
