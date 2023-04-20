## Test1: derive mamimum temperature records"

test_that("derive_param_maxtemp Test1: derive mamimum temperature records", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
    "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 1, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 2, 36.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 1, 38.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 2, 37.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 1, 36.1, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.4, "Occurrence Indicator", "OCCUR"
  )

  expected1 <- input %>%
    filter(FAOBJ == "FEVER") %>%
    arrange(USUBJID, FAOBJ, ATPTREF, AVALC, VSSTRESN, ATPTN) %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    dplyr::slice_tail(n = 1) %>%
    mutate(
      AVAL = VSSTRESN,
      AVALC = as.character(AVAL),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXTEMP",
      FATEST = "Maximum temperature"
    ) %>%
    select(USUBJID, FAOBJ, ATPTREF, AVALC, FATEST, FATESTCD, AVAL, DTYPE)

  expected_output <- bind_rows(input, expected1)

  actual_output <- derive_param_maxtemp(
    input,
    filter_faobj = "FEVER",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    test_maxtemp = "Maximum temperature",
    testcd_maxtemp = "MAXTEMP"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST",
      "FATESTCD", "VSSTRESN", "ATPTN"
    )
  )
})

## Test2:derive mamimum temperature records if more than one record has
## same maximum value within by group
test_that("derive_param_maxtemp Test2:derive mamimum temperature records if more than one record has
          same maximum value within by group", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
    "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 3, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 1, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 2, 36.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 3, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 1, 38.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 2, 37.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 3, 36.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 1, 36.1, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.4, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.7, "Occurrence Indicator", "OCCUR"
  )

  expected1 <- input %>%
    filter(FAOBJ == "FEVER") %>%
    arrange(USUBJID, FAOBJ, ATPTREF, AVALC, VSSTRESN, ATPTN) %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    dplyr::slice_tail(n = 1) %>%
    mutate(
      AVAL = VSSTRESN,
      AVALC = as.character(AVAL),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXTEMP",
      FATEST = "Maximum temperature"
    ) %>%
    select(USUBJID, FAOBJ, ATPTREF, AVALC, FATEST, FATESTCD, AVAL, DTYPE)

  expected_output <- bind_rows(input, expected1)

  actual_output <- derive_param_maxtemp(
    input,
    filter_faobj = "FEVER",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    test_maxtemp = "Maximum temperature",
    testcd_maxtemp = "MAXTEMP"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST",
      "FATESTCD", "VSSTRESN", "ATPTN"
    )
  )
})

## Test3:Check if the argument `filter_faobj` filters only fever records
## for deriving maximum temperature records

test_that("derive_param_maxtemp Test3:Check if the argument `filter_faobj` filters
          only fever records for deriving maximum temperature records", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
    "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 1, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 2, 36.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 1, 38.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 2, 37.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 1, 36.1, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.4, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "REDNESS", "VACC 1", "3", 1, NA, "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", "VACC 1", "7", 2, NA, "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", "VACC 1", "5", 1, NA, "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", "VACC 1", "6", 2, NA, "Diameter", "DIAMETER"
  )

  expected1 <- input %>%
    filter(FAOBJ == "FEVER") %>%
    arrange(USUBJID, FAOBJ, ATPTREF, AVALC, VSSTRESN, ATPTN) %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    dplyr::slice_tail(n = 1) %>%
    mutate(
      AVAL = VSSTRESN,
      AVALC = as.character(AVAL),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXTEMP",
      FATEST = "Maximum temperature"
    ) %>%
    select(USUBJID, FAOBJ, ATPTREF, AVALC, FATEST, FATESTCD, AVAL, DTYPE)

  expected_output <- bind_rows(input, expected1)

  actual_output <- derive_param_maxtemp(
    input,
    filter_faobj = "FEVER",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    test_maxtemp = "Maximum temperature",
    testcd_maxtemp = "MAXTEMP"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST",
      "FATESTCD", "VSSTRESN", "ATPTN"
    )
  )
})

## Test4:Change by group variables and check if the results are matching

test_that("derive_param_maxtemp Test4:Change by group variables and check if the
          results are matching", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
    "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 1, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 2, 36.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 1, 38.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 2, 37.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 1, 36.1, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.4, "Occurrence Indicator", "OCCUR"
  )

  expected1 <- input %>%
    filter(FAOBJ == "FEVER") %>%
    arrange(USUBJID, FAOBJ, ATPTREF, AVALC, VSSTRESN, ATPTN) %>%
    group_by(USUBJID, ATPTREF) %>%
    dplyr::slice_tail(n = 1) %>%
    mutate(
      AVAL = VSSTRESN,
      AVALC = as.character(AVAL),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXTEMP",
      FATEST = "Maximum temperature"
    ) %>%
    select(USUBJID, FAOBJ, ATPTREF, AVALC, FATEST, FATESTCD, AVAL, DTYPE)

  expected_output <- bind_rows(input, expected1)

  actual_output <- derive_param_maxtemp(
    input,
    filter_faobj = "FEVER",
    by_vars = exprs(USUBJID, ATPTREF),
    test_maxtemp = "Maximum temperature",
    testcd_maxtemp = "MAXTEMP"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST",
      "FATESTCD", "VSSTRESN", "ATPTN"
    )
  )
})

## Test5:Check if the arguments `test_maxtemp`,`testcd_maxtemp` works fine

test_that("derive_param_maxtemp Test5:Check if the arguments `test_maxtemp`,`testcd_maxtemp`
          works fine ", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
    "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 1, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 2, 36.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 1, 38.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 2, 37.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 1, 36.1, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.4, "Occurrence Indicator", "OCCUR"
  )

  expected1 <- input %>%
    filter(FAOBJ == "FEVER") %>%
    arrange(USUBJID, FAOBJ, ATPTREF, AVALC, VSSTRESN, ATPTN) %>%
    group_by(USUBJID, ATPTREF) %>%
    dplyr::slice_tail(n = 1) %>%
    mutate(
      AVAL = VSSTRESN,
      AVALC = as.character(AVAL),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXTEMPERATURE",
      FATEST = "Maximum temp"
    ) %>%
    select(USUBJID, FAOBJ, ATPTREF, AVALC, FATEST, FATESTCD, AVAL, DTYPE)

  expected_output <- bind_rows(input, expected1)

  actual_output <- derive_param_maxtemp(
    input,
    filter_faobj = "FEVER",
    by_vars = exprs(USUBJID, ATPTREF),
    test_maxtemp = "Maximum temp",
    testcd_maxtemp = "MAXTEMPERATURE"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST",
      "FATESTCD", "VSSTRESN", "ATPTN"
    )
  )
})

## Test6:Check if getting the expected error while passing invalid faobj
## in filter_faobj argument

test_that("derive_param_maxtemp Test6:Check if getting the expected error while
           passing invalid faobj in filter_faobj argument ", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
    "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 1, 37.3, "Occurrence Indicator", "OCCUR",
    "XYZ1001", "FEVER", "VACC 2", "Y", 2, 36.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 1, 38.2, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 1", "Y", 2, 37.5, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 1, 36.1, "Occurrence Indicator", "OCCUR",
    "XYZ1002", "FEVER", "VACC 2", "N", 2, 36.4, "Occurrence Indicator", "OCCUR"
  )

  expect_error(
    actual_output <- derive_param_maxtemp(
      input,
      filter_faobj = "REDNESS",
      by_vars = exprs(USUBJID, ATPTREF),
      test_maxtemp = "Maximum temp",
      testcd_maxtemp = "MAXTEMPERATURE"
    ),
    regexp = paste(
      "REDNESS doesn't exist in the FAOBJ variable."
    )
  )
})
