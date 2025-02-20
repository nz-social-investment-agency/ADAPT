################################################################################
#' Notes
#'  
################################################################################

## suppression_format_extract(string) ------------------------------------- ----

test_that("valid text works as expected", {
  
  actual = suppression_format_extract("col < 1")
  expected = list(input = "col < 1", column = "col", sign = "<", threshold = 1, valid = TRUE)
  expect_equal(actual, expected)
  
  # was originally "text_name <= 10" but equality removed
  actual = suppression_format_extract("text_name < 10")
  expected = list(input = "text_name < 10", column = "text_name", sign = "<", threshold = 10, valid = TRUE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("002 > .name.")
  expected = list(input = "002 > .name.", column = ".name.", sign = ">", threshold = 2, valid = TRUE)
  expect_equal(actual, expected)
  
  # was originally " 6 >= text " but equality removed
  actual = suppression_format_extract(" 6 > text ")
  expected = list(input = " 6 > text ", column = "text", sign = ">", threshold = 6, valid = TRUE)
  expect_equal(actual, expected)
  
})

test_that("invalid text fails", {
  
  actual = suppression_format_extract("col < one")
  expected = list(input = "col < one", column = "col", sign = "<", threshold = NA_integer_, valid = FALSE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("text_name <> 10")
  expected = list(input = "text_name <> 10", column = NA_character_, sign = "<>", threshold = NA_integer_, valid = FALSE)
  expect_equal(actual, expected)
  
  actual = suppression_format_extract("rubbish text")
  expected = list(input = "rubbish text", column = NA_character_, sign = "", threshold = NA_integer_, valid = FALSE)
  expect_equal(actual, expected)

})

## apply_random_rounding(input_array, base, seeds, threshold) ------------- ----

test_that("random rounding matches base",{
  # arrange
  input = 1:100
  # act
  rr3 = apply_random_rounding(input, base = 3)
  rr4 = apply_random_rounding(input, base = 4)
  rr7 = apply_random_rounding(input, base = 7)
  # assert
  expect_true(all(rr3 %% 3 == 0))
  expect_true(all(rr4 %% 4 == 0))
  expect_true(all(rr7 %% 7 == 0))
})

test_that("random rounding is within base of original", {
  # arrange
  input = 1:100
  # act
  rr3 = apply_random_rounding(input, base = 3)
  rr4 = apply_random_rounding(input, base = 4)
  rr7 = apply_random_rounding(input, base = 7)
  # assert
  expect_true(max(abs(rr3 - input)) < 3)
  expect_true(max(abs(rr4 - input)) < 4)
  expect_true(max(abs(rr7 - input)) < 7)
})

test_that("random rounding is distributed correctly", {
  # arrange
  LENG = 10000
  input = 1:LENG
  
  # act - base 3
  rr3 = apply_random_rounding(input, base = 3)
  diff3 = abs(rr3 - input)
  diff_of_0 = sum(diff3 == 0)
  diff_of_1 = sum(diff3 == 1)
  diff_of_2 = sum(diff3 == 2)
  denom = diff_of_1 + diff_of_2
  
  # assert - base 3
  expect_true(0.26 < diff_of_0 / LENG & diff_of_0 / LENG < 0.40)
  expect_true(0.59 < diff_of_1 / denom & diff_of_1 / denom < 0.73)
  expect_true(0.26 < diff_of_2 / denom & diff_of_2 / denom < 0.40)
  
  # act - base 7
  rr7 = apply_random_rounding(input, base = 7)
  diff7 = abs(rr7 - input)
  # remainders
  remain_of_0 = sum(input %% 7 == 0)
  remain_of_1 = sum(input %% 7 == 1)
  remain_of_2 = sum(input %% 7 == 2)
  remain_of_3 = sum(input %% 7 == 3)
  remain_of_4 = sum(input %% 7 == 4)
  remain_of_5 = sum(input %% 7 == 5)
  remain_of_6 = sum(input %% 7 == 6)
  # difference for each remainder
  remain0_diff0 = sum(input %% 7 == 0 & diff7 == 0)
  remain1_diff1 = sum(input %% 7 == 1 & diff7 == 1)
  remain1_diff6 = sum(input %% 7 == 1 & diff7 == 6)
  remain2_diff2 = sum(input %% 7 == 2 & diff7 == 2)
  remain2_diff5 = sum(input %% 7 == 2 & diff7 == 5)
  remain3_diff3 = sum(input %% 7 == 3 & diff7 == 3)
  remain3_diff4 = sum(input %% 7 == 3 & diff7 == 4)
  remain4_diff3 = sum(input %% 7 == 4 & diff7 == 3)
  remain4_diff4 = sum(input %% 7 == 4 & diff7 == 4)
  remain5_diff2 = sum(input %% 7 == 5 & diff7 == 2)
  remain5_diff5 = sum(input %% 7 == 5 & diff7 == 5)
  remain6_diff1 = sum(input %% 7 == 6 & diff7 == 1)
  remain6_diff6 = sum(input %% 7 == 6 & diff7 == 6)
  # remainders and difference match
  expect_equal(LENG, remain0_diff0 + remain1_diff1 + remain1_diff6 + remain2_diff2 + remain2_diff5 +
                 remain3_diff3 + remain3_diff4 + remain4_diff3 + remain4_diff4 +
                 remain5_diff2 + remain5_diff5 + remain6_diff1 + remain6_diff6)
  # assert - base 7
  expect_true(1.00 <= remain0_diff0 / remain_of_0 & remain0_diff0 / remain_of_0 <= 1.00)
  expect_true(0.80 <= remain1_diff1 / remain_of_1 & remain1_diff1 / remain_of_1 <= 0.91)
  expect_true(0.09 <= remain1_diff6 / remain_of_1 & remain1_diff6 / remain_of_1 <= 0.20)
  expect_true(0.68 <= remain2_diff2 / remain_of_2 & remain2_diff2 / remain_of_2 <= 0.74)
  expect_true(0.26 <= remain2_diff5 / remain_of_2 & remain2_diff5 / remain_of_2 <= 0.32)
  expect_true(0.54 <= remain3_diff3 / remain_of_3 & remain3_diff3 / remain_of_3 <= 0.61)
  expect_true(0.39 <= remain3_diff4 / remain_of_3 & remain3_diff4 / remain_of_3 <= 0.46)
  expect_true(0.54 <= remain4_diff3 / remain_of_4 & remain4_diff3 / remain_of_4 <= 0.61)
  expect_true(0.39 <= remain4_diff4 / remain_of_4 & remain4_diff4 / remain_of_4 <= 0.46)
  expect_true(0.68 <= remain5_diff2 / remain_of_5 & remain5_diff2 / remain_of_5 <= 0.74)
  expect_true(0.26 <= remain5_diff5 / remain_of_5 & remain5_diff5 / remain_of_5 <= 0.32)
  expect_true(0.80 <= remain6_diff1 / remain_of_6 & remain6_diff1 / remain_of_6 <= 0.91)
  expect_true(0.09 <= remain6_diff6 / remain_of_6 & remain6_diff6 / remain_of_6 <= 0.20)
})

test_that("negative numbers rounded", {
  input = -1 * (1:100)
  
  rr4 = apply_random_rounding(input, base = 4)
  
  expect_true(all(rr4 %% 4 == 0))
  expect_true(all(abs(rr4 - input) < 4))

})

test_that("random rounding can be stable", {
  # arrange
  input = 1:10000
  seeds = 123 + 1:10000
  
  # act
  rr3a = apply_random_rounding(input, base = 3, seeds = seeds)
  rr3b = apply_random_rounding(input, base = 3, seeds = seeds)
  rr3c = apply_random_rounding(input, base = 3)
  
  # assert
  expect_true(all(rr3a == rr3b))
  expect_false(all(rr3a == rr3c))
})

test_that("rounding respects thresholds", {
  
  input = rep(c(5,6,19,20), 100)
  actual_rr3 = apply_random_rounding(input, threshold = c(6,20))
  expected_rr3 = rep(c(3,6,18,21), 100)
  expect_equal(actual_rr3, expected_rr3)
  
  input = rep(c(11:19), 100)
  actual_rr10 = apply_random_rounding(input, base = 10, threshold = 15)
  expected_rr10 = rep(c(10,10,10,10,20,20,20,20,20), 100)
  expect_equal(actual_rr10, expected_rr10)
})

## apply_graduated_random_rounding(input_array, seeds, threshold) --------- ----

test_that("graduated random rounding applied", {
  # arrange
  count0_18 = sample(1:18, 100, replace = TRUE)
  count19 = rep(19, 100)
  count20_99 = sample(20:99, 100, replace = TRUE)
  count100_999 = sample(100:999, 100, replace = TRUE)
  count1000 = sample(1000:3000, 100, replace = TRUE)
  
  # act
  output_0_18 = apply_graduated_random_rounding(count0_18)
  output_19 = apply_graduated_random_rounding(count19)
  output_20_99 = apply_graduated_random_rounding(count20_99)
  output_100_999 = apply_graduated_random_rounding(count100_999)
  output_1000 = apply_graduated_random_rounding(count1000)
  
  # assert
  expect_true(all(output_0_18 %% 3 == 0))
  expect_true(all(output_19 %% 2 == 0))
  expect_true(all(output_20_99 %% 5 == 0))
  expect_true(all(output_100_999 %% 10 == 0))
  expect_true(all(output_1000 %% 100 == 0))
  
  expect_true(all(abs(output_0_18 - count0_18) < 3))
  expect_true(all(abs(output_19 - count19) < 2))
  expect_true(all(abs(output_20_99 - count20_99) < 5))
  expect_true(all(abs(output_100_999 - count100_999) < 10))
  expect_true(all(abs(output_1000 - count1000) < 100))
})

test_that("graduated rounding can be stable", {
  # arrange
  count = rep(1:100, 5)
  seeds = sample(1:100, length(count), replace = TRUE)
  
  # act
  output_1 = apply_graduated_random_rounding(count)
  output_2 = apply_graduated_random_rounding(count)
  output_s1 = apply_graduated_random_rounding(count, seeds = seeds)
  output_s2 = apply_graduated_random_rounding(count, seeds = seeds)
  
  # assert
  expect_false(all(output_1 == output_2))
  expect_true(all(output_s1 == output_s2))
})

test_that("graduate rounding respects thresholds", {
  
  v1 = c(5,6,19,20,51,54,96, 99,101,107,313,318)
  v2 = c(3,6,18,20,50,55,95,100,100,110,310,320)
  tt = c(6,20,52,97,106,315)
  
  input = rep(v1, 100)
  actual_grr = apply_graduated_random_rounding(input, threshold = tt)
  expected_grr = rep(v2, 100)
  expect_equal(actual_grr, expected_grr)
  
})

## apply_conventional_rounding(input_array, base) ------------------------- ----

test_that("rounding matches base", {
  
  input = 0:10
  expected = c(0,0,2,4,4,4,6,8,8,8,10)
  
  actual = apply_conventional_rounding(input, 2)
  expect_equal(actual, expected)
  
})

test_that("various bases work", {
  
  input = 0:20
  
  actual = apply_conventional_rounding(input, 3)
  expected = c(0,0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21)
  expect_equal(actual, expected)
  
  actual = apply_conventional_rounding(input, 5)
  expected = c(0,0,0,5,5,5,5,5,10,10,10,10,10,15,15,15,15,15,20,20,20)
  expect_equal(actual, expected)
  
  actual = apply_conventional_rounding(input, 10)
  expected = c(0,0,0,0,0,0,10,10,10,10,10,10,10,10,10,20,20,20,20,20,20)
  expect_equal(actual, expected)
  
})

## apply_small_count_suppression(input_array, with, threshold, treat_na_as) ----

test_that("suppression occurs", {
  # arrange
  count = sample(1:14, 100, replace = TRUE)
  
  expected = count
  expected[count < 6] = NA
  
  # act
  actual = apply_small_count_suppression(count, count, threshold = 6)
  # assert
  expect_equal(actual, expected)
})

test_that("suppressing with different arrays work", {
  # arrange
  count = sample(1:30, 100, replace = TRUE)
  sum = sample(100:200, 100, replace = TRUE)
  
  expected = sum
  expected[count < 20] = NA
  
  # act
  actual = apply_small_count_suppression(sum, count, 20)
  # assert
  expect_equal(actual, expected)
})

test_that("single 'with' can be used", {
  
  input = sample(1:30, 100, replace = TRUE)
  
  # no suppression with >= threshold
  actual = apply_small_count_suppression(input, with = 20, threshold = 20)
  expect_equal(actual, input)
  
  # full suppression with < threshold
  actual = apply_small_count_suppression(input, with = 20 - 1E-12, threshold = 20)
  expect_equal(actual, rep(NA_integer_, length(input)))
})

test_that("threshold array works", {
  # single with value
  input = sample(1:30, 100, replace = TRUE)
  with = 20
  threshold = sample(10:30, 100, replace = TRUE)
  
  actual = apply_small_count_suppression(input, with, threshold)
  
  expected = input
  expected[with < threshold] = NA_integer_
  
  expect_equal(actual, expected)
  
  # array with
  input = sample(1:30, 100, replace = TRUE)
  with = sample(5:35, 100, replace = TRUE)
  threshold = sample(10:30, 100, replace = TRUE)
  
  actual = apply_small_count_suppression(input, with, threshold)
  
  expected = input
  expected[with < threshold] = NA_integer_
  
  expect_equal(actual, expected)
})

test_that("non-numeric input can be suppressed", {
  input = sample(c("a","b","c"), 100, replace = TRUE)
  count = sample(10:30, 100, replace = TRUE)
  
  expected = input
  expected[count < 15] = NA
  
  actual = apply_small_count_suppression(input, count, threshold = 15)
  expect_equal(actual, expected)
})

test_that("missing values cause suppression", {
  
  input = sample(100:200, 100, replace = TRUE)
  count = sample(c(NA,10,100), 100, replace = TRUE)
  
  # low threshold no NA handling
  actual = apply_small_count_suppression(input, count, threshold = 6)
  expected = input
  expected[is.na(count)] = NA
  expect_equal(actual, expected)
  
  # high threshold no NA handling
  actual = apply_small_count_suppression(input, count, threshold = 20)
  expected = input
  expected[is.na(count) | count == 10] = NA
  expect_equal(actual, expected)
  
  # low threshold, NA --> high
  actual = apply_small_count_suppression(input, count, threshold = 6, treat_na_as = 99)
  expected = input
  expect_equal(actual, expected)
  
  # low threshold, NA --> low
  actual = apply_small_count_suppression(input, count, threshold = 6, treat_na_as = 1)
  expected = input
  expected[is.na(count)] = NA
  expect_equal(actual, expected)
  
  # high threshold, NA --> high
  actual = apply_small_count_suppression(input, count, threshold = 20, treat_na_as = 99)
  expected = input
  expected[!is.na(count) & count == 10] = NA
  expect_equal(actual, expected)
  
  # high threshold, NA --> low
  actual = apply_small_count_suppression(input, count, threshold = 20, treat_na_as = 6)
  expected = input
  expected[is.na(count) | count == 10] = NA
  expect_equal(actual, expected)
})

## create_stable_seeds(source_values, stable_above) ----------------------- ----

test_that("table values generated", {
  input = rep(sample(31:1000, 100, replace = TRUE), 20)
  actual = create_stable_seeds(input)
  expect_true(length(unique(actual)) < 100)
})

test_that("low values are not stable", {
  input = sample(0:29, 1000, replace = TRUE)
  actual = create_stable_seeds(input)
  expect_true(length(unique(actual)) > 500)
})

test_that("stable_above control changes output", {
  input= rep(10, 100)
  
  actual = create_stable_seeds(input, stable_above = 11)
  expect_true(length(unique(actual)) > 50)
  
  actual = create_stable_seeds(input, stable_above = 9)
  expect_true(length(unique(actual)) == 1)
})

test_that("NA values get seeds", {
  input = c(NA,1,10,100,NA,1000)
  actual = create_stable_seeds(input)
  expect_false(any(is.na(actual)))
})
