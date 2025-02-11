################################################################################
#' Notes
#' 
################################################################################

## Extract components of suppression instructions ------------------------- ----
#' Decompose suppression instructions into individual components and validate
#' correctness of instructions.
#'
#' @param string a text string with the suppression instructions to process.
#'
#' @return a list with five components:
#' * `input` the original input string.
#' * `column` the column to process.
#' * `sign` the inequality that defines suppression.
#' * `threshold` the numeric value to suppress below.
#' * `valid` T/F whether the decomposed input makes for valid suppression rules.
#' @md
#'
#' @details
#' Splits an input string into three components and then evaluates whether these
#' components make valid suppression instructions.
#' 
#' For example: "colname < 6" will be decomposed into column = "colname", sign
#' = "<", and threshold = "6".
#' 
suppression_format_extract = function(string){
  stopifnot(is.character(string))
  
  # remove spaces
  input = string
  string = gsub(" ", "", string)
  
  # sign = string with all non-<=> characters removed
  sign = gsub("[^<>]", "", string)
  
  # column and threshold are on either size of sign
  split = strsplit(string, sign)[[1]]
  if(grepl("<", sign)){
    column = split[1]
    threshold = split[2]
  } else if(grepl(">", sign)) {
    column = split[2]
    threshold = split[1]
  } else {
    column = NA_character_
    threshold = NA_character_
  }
  threshold = suppressWarnings(as.numeric(threshold))
  
  # check validity
  valid = sign %in% c("<", ">") &
    !is.na(threshold) &
    nchar(column) >= 1 
  
  # return
  return(list(input = input, column = column, sign = sign, threshold = threshold, valid = valid))
}

## Random rounding -------------------------------------------------------- ----
#' Apply random rounding to an array
#' 
#' @param input_array a numeric array to randomly round.
#' @param base the base to round to. defaults to 3. (RR3)
#' @param seeds an optional column of seed values, with length equal to
#' `input_array`. If provided, these seeds are used to determine the random
#' rounding - useful for consistent or reproducible rounding.
#' @param threshold If provided, where `input_array` is at least/below the
#' `threshold`, then the output will also be at least/below the threshold.
#' Can override randomness of rounding. Most common use case is to ensure
#' rounding is consistent with suppression.
#' 
#' @return the input vector with all values randomly rounded so they are
#' divisible by `base`.
#' 
#' @details
#' Randomly rounds a numeric vector to the given base.
#' For example, if base is 3 (default) then:
#' * 3 & 6 will be left as is because they are already base 3;
#' * 4 & 7 will be rounded down with prob 2/3 and up with prob 1/3;
#' * 5 & 8 will be rounded down with prob 1/3 and up with prob 2/3.
#' @md
#' 
#' @export
apply_random_rounding = function(input_array, base = 3, seeds = NULL, threshold = numeric()){
  # check vector is numeric
  stopifnot(is.numeric(input_array))
  stopifnot(is.numeric(base))
  stopifnot(is.numeric(threshold))
  stopifnot(all(diff(sort(threshold)) >= base)) # too small diff --> thresholds contradiction
  
  # seeds
  if(is.null(seeds)){
    probs = stats::runif(length(input_array))
  } else {
    stopifnot(is.numeric(seeds))
    stopifnot(length(input_array) == length(seeds))
    seeds = dplyr::coalesce(seeds, 0)
    probs = sapply(seeds, function(s){set.seed(s); stats::runif(1)})
  }
  
  # apply random rounding
  remainder_vector = input_array %% base
  prob_round_down = (base - remainder_vector) / base
  ind_round_down = probs < prob_round_down
  
  output_array = input_array + base - remainder_vector - base * ind_round_down
  
  # enforce rounding consistency
  for(tt in threshold){
    need_round_up = input_array >= tt & output_array < tt
    output_array[need_round_up] = output_array[need_round_up] + base
    
    need_round_down = input_array < tt & output_array >= tt
    output_array[need_round_down] = output_array[need_round_down] - base
  }

  return(output_array)
}

## Graduated random rounding ---------------------------------------------- ----
#' Apply graduated random rounding (GRR) to an array
#' 
#' @param input_array a numeric array to randomly round.
#' @param seeds an optional column of seed values, with length equal to
#' `input_array`. If provided, these seeds are used to determine the random
#' rounding - useful for consistent or reproducible rounding.
#' @param threshold If provided, where `input_array` is at least/below the
#' `threshold`, then the output will also be at least/below the threshold.
#' Can override randomness of rounding. Most common use case is to ensure
#' rounding is consistent with suppression.
#' 
#' @return the input vector with all values graduated randomly rounded.
#' 
#' @details
#' Thresholds for graduated rounding are set by the Stats NZ Microdata output
#' guide. Take the absolute value of `input_array` and for each value apply
#' the first applicable rule:
#' * Values less than 19 are randomly rounded to base 3
#' * Values less than 20 are randomly rounded to base 2
#' * Values less than 100 are randomly rounded to base 5
#' * Values less than 1000 are randomly rounded to base 10
#' * Values greater than 1000 are randomly rounded to base 100
#' @md
#' 
#' @export
apply_graduated_random_rounding = function(input_array, seeds = NULL, threshold = numeric()){
  # check vector is numeric
  stopifnot(is.numeric(input_array))

  # seeds
  if(is.null(seeds)){
    probs = stats::runif(length(input_array))
  } else {
    stopifnot(is.numeric(seeds))
    stopifnot(length(input_array) == length(seeds))
    seeds = dplyr::coalesce(seeds, 0)
    probs = sapply(seeds, function(s){set.seed(s); stats::runif(1)})
  }
  
  # abs * sign
  abs_input = abs(input_array)
  sign_input = sign(input_array)
  
  # create output
  output = rep(NA, length(input_array))
  
  # round 0 <= value < 19 requires base 3
  in_range = 0 <= abs_input & abs_input < 19
  threshold_range = 0 <= threshold & threshold <= 21
  rounded = apply_random_rounding(abs_input[in_range], base = 3, seeds = seeds[in_range], threshold = threshold[threshold_range])
  output[in_range] = rounded * sign_input[in_range]
  
  # round 19 <= value < 20 requires base 2
  in_range = 19 <= abs_input & abs_input < 20
  threshold_range = 19 <= threshold & threshold <= 20
  rounded = apply_random_rounding(abs_input[in_range], base = 2, seeds = seeds[in_range], threshold = threshold[threshold_range])
  output[in_range] = rounded * sign_input[in_range]
  
  # round 20 <= value < 100 requires base 5
  in_range = 20 <= abs_input & abs_input < 100
  threshold_range = 20 <= threshold & threshold <= 100
  rounded = apply_random_rounding(abs_input[in_range], base = 5, seeds = seeds[in_range], threshold = threshold[threshold_range])
  output[in_range] = rounded * sign_input[in_range]
  
  # round 100 <= value < 1000 requires base 10
  in_range = 100 <= abs_input & abs_input < 1000
  threshold_range = 100 <= threshold & threshold <= 1000
  rounded = apply_random_rounding(abs_input[in_range], base = 10, seeds = seeds[in_range], threshold = threshold[threshold_range])
  output[in_range] = rounded * sign_input[in_range]
  
  # round 1000 <= value requires base 100
  in_range = 1000 <= abs_input
  threshold_range = 1000 <= threshold
  rounded = apply_random_rounding(abs_input[in_range], base = 100, seeds = seeds[in_range], threshold = threshold[threshold_range])
  output[in_range] = rounded * sign_input[in_range]
  
  # done
  return(output)
}

## Conventional rounding -------------------------------------------------- ----
#' Apply conventional rounding to an array
#' 
#' @param input_array a numeric array to randomly round.
#' @param base the base to round to.
#' 
#' @return the input vector with all values rounded so they are
#' divisible by `base`.
#' 
#' @details
#' Consistent with `round` in base R (which implements the IEC 60559 standard)
#' values of 0.5 are 'rounded to the even digit'. This is different from
#' simple rounding where values of 0.5 are rounded up.
#' 
#' @export
apply_conventional_rounding = function(input_array, base){
  # check vector is numeric
  stopifnot(is.numeric(input_array))
  stopifnot(is.numeric(base))
  stopifnot(base > 0)
  
  output_array = round(input_array / base) * base

  return(output_array)
}

## Suppress small counts -------------------------------------------------- ----
#' Apply small count suppression to an array
#' 
#' @param input_array an array to suppress.
#' @param with a numeric array to compare against `threshold` to determine
#' whether or not to suppress. Either length 1 or length equal to `input_array`.
#' @param threshold the minimum accepted value of `with` that will not result
#' in suppression. Either length 1 or length equal to `input_array`.
#' @param treat_na_as optional, numeric value to replace missing values.
#' Defaults to 0, so NA values in `with` will suppress `input_array`.
#' See details. Either length 1 or length equal to `input_array`.
#' 
#' @return The `input_array` with values suppressed (replaced by NA) anywhere
#' that `with >= threshold` is not true.
#' 
#' @details
#' This function takes the most conservative approach and suppresses all values
#' where there is not evidence that the count is at least the threshold. This
#' means than missing values in `with` will suppress `input_array`.
#' 
#' If missing values in `with` reflect that no suppression check is required,
#' then `treat_na_as = 9999` or similar is advised.
#' 
#' Missing values in `threshold` or `treat_na_as` will cause errors.
#' 
#' @export
apply_small_count_suppression = function(input_array, with, threshold, treat_na_as = 0){
  input_length = length(input_array)
  stopifnot(is.numeric(with))
  stopifnot(length(with) %in% c(1, input_length))
  stopifnot(is.numeric(threshold))
  stopifnot(length(threshold) %in% c(1, input_length))
  stopifnot(all(!is.na(threshold)))
  stopifnot(is.numeric(treat_na_as))
  stopifnot(length(treat_na_as) %in% c(1, input_length))
  stopifnot(all(!is.na(treat_na_as)))
  
  # setup inputs
  with = rep(with, length.out = input_length)
  with = dplyr::coalesce(with, treat_na_as)
  threshold = rep(threshold, length.out = input_length)
  
  output = rep(NA_integer_, input_length)
  passes = with >= threshold
  output[passes] = input_array[passes]
  
  return(output)
}

## Create consistent seeds ------------------------------------------------ ----
#' Create seeds for stable random rounding
#' 
#' @param source_values Numeric vector of values to round for which stable
#' random rounding is required.
#' @param stable_above Minimum source value for which consistent seeds are
#' required. Defaults to 30, because in practice smaller values come from
#' different sources.
#'  
#' @return Numeric vector of the same length as `source_values` suitable for
#' in `apply_random_rounding` or `apply_graduated_random_rounding` as `seeds`.
#' Output values will be identical where `source_values` and identical and
#' greater than `stable_above`.
#' 
create_stable_seeds = function(source_values, stable_above = 30){
  stopifnot(is.numeric(source_values))
  stopifnot(is.numeric(stable_above))
  stopifnot(length(stable_above) == 1)
  
  # missing values get 0
  source_values = dplyr::coalesce(source_values, 0)
  
  # unique high values
  high_values = unique(source_values[source_values >= stable_above])
  possible_seeds = round(1000 * stats::runif(length(high_values)))
  
  output = sapply(
    source_values,
    function(x){
      ifelse(x < stable_above, round(1000 * stats::runif(1)), possible_seeds[x == high_values])
  })
  
  return(output)
}
