## Test 1: Derive `FATEST`,`FATESTCD` indicating severity for the event `REDNESS`

test_that("derive_diam_to_sev_records Test 1: Derive `FATEST`,`FATESTCD` indicating
          severity for the event `REDNESS`", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
    "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 11, "11", "VACCINATION 1", "Diameter", "DIAMETER"
  )

  format_avalc <- function(x) {
    case_when(
      dplyr::between(x, 0, 2) ~ "NONE",
      dplyr::between(x, 2, 5) ~ "MILD",
      dplyr::between(x, 5, 10) ~ "MODERATE",
      x > 10 ~ "SEVERE"
    )
  }

  expected1 <- input %>%
    mutate(
      FATEST = "Severity/Intensity",
      FATESTCD = "SEV",
      AVALC = format_avalc(AVAL),
      FASEQ = NA_integer_
    )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }
  expected_output <- bind_rows(input, expected1 %>%
    mutate(AVAL = format_aval(AVALC)))

  actual_output <- derive_diam_to_sev_records(
    dataset = input,
    diam_code = "DIAMETER",
    faobj_values = c("REDNESS"),
    testcd_sev = "SEV",
    test_sev = "Severity/Intensity",
    none = 0,
    mild = 2,
    mod = 5,
    sev = 10
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST", "FATESTCD")
  )
})

## Test 2: Derive `FATEST`,`FATESTCD` indicating severity for the event `REDNESS` & `SWELLING`

test_that("derive_diam_to_sev_records Test 2: Derive `FATEST`,`FATESTCD` indicating
          severity for the event `REDNESS` & `SWELLING`", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
    "XYZ1001", "REDNESS", 1.5, "1.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 11, "11", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "SWELLING", 6.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "SWELLING", 4.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER"
  )

  format_avalc <- function(x) {
    case_when(
      dplyr::between(x, 0, 2) ~ "NONE",
      dplyr::between(x, 2, 5) ~ "MILD",
      dplyr::between(x, 5, 10) ~ "MODERATE",
      x > 10 ~ "SEVERE"
    )
  }

  expected1 <- input %>%
    mutate(
      FATEST = "Severity",
      FATESTCD = "SEV",
      AVALC = format_avalc(AVAL),
      FASEQ = NA_integer_
    )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }
  expected_output <- bind_rows(input, expected1 %>%
    mutate(AVAL = format_aval(AVALC)))

  actual_output <- derive_diam_to_sev_records(
    dataset = input,
    diam_code = "DIAMETER",
    faobj_values = c("REDNESS", "SWELLING"),
    testcd_sev = "SEV",
    test_sev = "Severity",
    none = 0,
    mild = 2,
    mod = 5,
    sev = 10
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST", "FATESTCD")
  )
})

## Test 3: Check if the arguments `none`,`mild`,`moderate`,`sev` works correctly"

test_that("derive_diam_to_sev_records Test 3: Check if the arguments `none`,
          `mild`,`moderate`,`sev` works correctly", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
    "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 10, "10", "VACCINATION 1", "Diameter", "DIAMETER"
  )

  format_avalc <- function(x) {
    case_when(
      dplyr::between(x, 0, 3) ~ "NONE",
      dplyr::between(x, 3, 6) ~ "MILD",
      dplyr::between(x, 6, 9) ~ "MODERATE",
      x > 9 ~ "SEVERE"
    )
  }

  expected1 <- input %>%
    mutate(
      FATEST = "Severity/Intensity",
      FATESTCD = "SEV",
      AVALC = format_avalc(AVAL),
      FASEQ = NA_integer_
    )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }
  expected_output <- bind_rows(input, expected1 %>%
    mutate(AVAL = format_aval(AVALC)))

  actual_output <- derive_diam_to_sev_records(
    dataset = input,
    diam_code = "DIAMETER",
    faobj_values = c("REDNESS"),
    testcd_sev = "SEV",
    test_sev = "Severity/Intensity",
    none = 0,
    mild = 3,
    mod = 6,
    sev = 9
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST", "FATESTCD")
  )
})

## Test 4: Check if the input dataset has severity records and remove those records correctly

test_that("derive_diam_to_sev_records Test 4: Check if the input dataset has
          severity records and remove those records correctly", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FAEVAL, ~ANL01FL,
    "XYZ1001", "REDNESS", 1.5, "1.5", "VACCINATION 1", "Diameter", "DIAMETER", "SUBJECT", "Y",
    "XYZ1001", "REDNESS", 11, "11", "VACCINATION 1", "Diameter", "DIAMETER", "SUBJECT", "Y",
    "XYZ1001", "SWELLING", 6.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER", "SUBJECT", "Y",
    "XYZ1001", "SWELLING", 4.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER", "SUBJECT", "Y",
    "XYZ1001", "REDNESS", 1.5, "NONE", "VACCINATION 1", "Severity", "SEV", "SUBJECT", "N",
    "XYZ1001", "REDNESS", 1.5, "NONE", "VACCINATION 1", "Severity", "SEV", "INVESTIGATOR", "Y",
    "XYZ1001", "REDNESS", 11, "SEVERE", "VACCINATION 1", "Severity", "SEV", "SUBJECT", "Y",
    "XYZ1001", "SWELLING", 6.5, "MODERATE", "VACCINATION 1", "Severity", "SEV", "SUBJECT", "Y",
    "XYZ1001", "SWELLING", 4.5, "MILD", "VACCINATION 1", "Severity", "SEV", "SUBJECT", "Y",
  )

  format_avalc <- function(x) {
    case_when(
      dplyr::between(x, 0, 2) ~ "NONE",
      dplyr::between(x, 2, 5) ~ "MILD",
      dplyr::between(x, 5, 10) ~ "MODERATE",
      x > 10 ~ "SEVERE"
    )
  }

  input <- input %>%
    filter(ANL01FL == "Y" & FATESTCD != "SEV" & FAOBJ %in% c("REDNESS", "SWELLING"))

  expected1 <- input %>%
    mutate(
      FATEST = "Severity",
      FATESTCD = "SEV",
      AVALC = format_avalc(AVAL),
      FASEQ = NA_integer_
    )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }
  expected_output <- bind_rows(input, expected1 %>%
    mutate(AVAL = format_aval(AVALC)))

  actual_output <- derive_diam_to_sev_records(
    dataset = input,
    filter_add = ANL01FL == "Y",
    diam_code = "DIAMETER",
    faobj_values = c("REDNESS", "SWELLING"),
    testcd_sev = "SEV",
    test_sev = "Severity",
    none = 0,
    mild = 2,
    mod = 5,
    sev = 10
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST", "FATESTCD")
  )
})

## Test 5: Check if the arguments `test_sev`,`testcd_sev` works correctly

test_that("derive_diam_to_sev_records Test 5: Check if the arguments `test_sev`,
          `testcd_sev` works correctly", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
    "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 11, "11", "VACCINATION 1", "Diameter", "DIAMETER"
  )

  format_avalc <- function(x) {
    case_when(
      dplyr::between(x, 0, 2) ~ "NONE",
      dplyr::between(x, 2, 5) ~ "MILD",
      dplyr::between(x, 5, 10) ~ "MODERATE",
      x > 10 ~ "SEVERE"
    )
  }

  expected1 <- input %>%
    mutate(
      FATEST = "Severity/Intensity/Sev",
      FATESTCD = "SEVERITY/SEV",
      AVALC = format_avalc(AVAL),
      FASEQ = NA_integer_
    )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }
  expected_output <- bind_rows(input, expected1 %>%
    mutate(AVAL = format_aval(AVALC)))

  actual_output <- derive_diam_to_sev_records(
    dataset = input,
    diam_code = "DIAMETER",
    faobj_values = c("REDNESS"),
    testcd_sev = "SEVERITY/SEV",
    test_sev = "Severity/Intensity/Sev",
    none = 0,
    mild = 2,
    mod = 5,
    sev = 10
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST", "FATESTCD")
  )
})

## Test 6: error is issued if the `diam_code` to be filtered is not in the input dataset

test_that("derive_diam_to_sev_records Test 6: Error is issued if the `diam_code`
          to be filtered is not in the input dataset", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
    "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 11, "11", "VACCINATION 1", "Diameter", "DIAMETER"
  )

  expect_warning(
    derive_diam_to_sev_records(
      dataset = input,
      diam_code = "DIAM",
      faobj_values = c("REDNESS"),
      testcd_sev = "SEVERITY/SEV",
      test_sev = "Severity/Intensity/Sev",
      none = 0,
      mild = 2,
      mod = 5,
      sev = 10
    ),
    regexp = paste(
      "DIAM doesn't exist in the filtered record"
    )
  )
})

## Test7: Check if the arguments `diam_code` works correctly if we pass multiple values

test_that("derive_diam_to_sev_records Test 7: Check if the arguments `diam_code` works correctly
          if we pass multiple values", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
    "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
    "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAM",
    "XYZ1001", "REDNESS", 10, "10", "VACCINATION 1", "Diameter", "DIAM"
  )

  format_avalc <- function(x) {
    case_when(
      dplyr::between(x, 0, 3) ~ "NONE",
      dplyr::between(x, 3, 6) ~ "MILD",
      dplyr::between(x, 6, 9) ~ "MODERATE",
      x > 9 ~ "SEVERE"
    )
  }

  expected1 <- input %>%
    mutate(
      FATEST = "Severity/Intensity",
      FATESTCD = "SEV",
      AVALC = format_avalc(AVAL),
      FASEQ = NA_integer_
    )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }
  expected_output <- bind_rows(input, expected1 %>%
    mutate(AVAL = format_aval(AVALC)))

  actual_output <- derive_diam_to_sev_records(
    dataset = input,
    diam_code = c("DIAMETER", "DIAM"),
    faobj_values = c("REDNESS"),
    testcd_sev = "SEV",
    test_sev = "Severity/Intensity",
    none = 0,
    mild = 3,
    mod = 6,
    sev = 9
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FAOBJ", "AVAL", "AVALC", "ATPTREF", "FATEST", "FATESTCD")
  )
})
