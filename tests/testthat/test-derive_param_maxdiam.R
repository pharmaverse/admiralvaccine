## Test 1: Check if a new record is created with Maximum diameter value"

testthat::test_that("derive_param_maxdiam Test 1: Check if a new record is
                    created with Maximum diameter value", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~FATEST, ~FATESTCD, ~FASCAT, ~FALNKGRP, ~FATPT,
    "ABC001", "REDNESS", "VACC 1", 1, "Diameter", "DIAM", "Admin", "REDNESS-VAC1", "DAY1",
    "ABC001", "REDNESS", "VACC 1", 2, "Diameter", "DIAM", "Admin", "REDNESS-VAC1", "DAY2",
    "ABC001", "REDNESS", "VACC 2", 7, "Diameter", "DIAM", "Admin", "REDNESS-VAC2", "DAY1",
    "ABC001", "REDNESS", "VACC 2", 8, "Diameter", "DIAM", "Admin", "REDNESS-VAC2", "DAY2"
  )


  temp <- input %>%
    group_by(USUBJID, FAOBJ, FASCAT, ATPTREF, FALNKGRP) %>%
    arrange(desc(AVAL), .by_group = TRUE) %>%
    summarise(AVAL = first(AVAL)) %>%
    mutate(
      FATEST = "Maximum Diameter",
      FATESTCD = "MAXDIAM",
      DTYPE = "MAXIMUM"
    )
  expected <- bind_rows(input, temp)

  actual <- derive_param_maxdiam(
    dataset = input,
    filter = FAOBJ == "REDNESS" & FATESTCD == "DIAM",
    by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
    test_maxdiam = "Maximum Diameter",
    testcd_maxdiam = "MAXDIAM"
  )

  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FAOBJ", "FALNKGRP", "FATPT"))
})

## Test 2:Check if filter condition and test/testcd values works fine

testthat::test_that("derive_param_maxdiam Test 2:Check if filter condition and
                    test/testcd values works fine", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~FATESTCD, ~FASCAT, ~FATPT, ~FALNKGRP,
    "ABC001", "REDNESS", "VACC 1", 1, "DIAM", "Admin site", "DAY 1", "REDNESS-VAC1",
    "ABC001", "REDNESS", "VACC 1", 2, "DIAM", "Admin site", "DAY 2", "REDNESS-VAC1",
    "ABC001", "Swelling", "VACC 1", 4, "DIAM", "Admin site", "DAY 1", "Swelling-VAC1",
    "ABC001", "Swelling", "VACC 1", 3, "DIAM", "Admin site", "DAY 2", "Swelling-VAC1",
    "ABC001", "Swelling", "VACC 2", 7, "DIAM", "Admin site", "DAY 1", "Swelling-VAC2",
    "ABC001", "Swelling", "VACC 2", 8, "DIAM", "Admin site", "DAY 2", "Swelling-VAC2"
  )


  temp <- input %>%
    group_by(USUBJID, FAOBJ, FASCAT, ATPTREF, FALNKGRP) %>%
    filter(FAOBJ %in% c("Redness", "Swelling") & FATESTCD == "DIAM") %>%
    arrange(desc(AVAL), .by_group = TRUE) %>%
    summarise(AVAL = first(AVAL)) %>%
    mutate(
      FATEST = "Max Diameter",
      FATESTCD = "MAXDIAM",
      DTYPE = "MAXIMUM"
    )
  expected <- bind_rows(input, temp)


  actual <- derive_param_maxdiam(
    dataset = input,
    filter = FAOBJ %in% c("Redness", "Swelling") & FATESTCD == "DIAM",
    by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
    test_maxdiam = "Max Diameter",
    testcd_maxdiam = "MAXDIAM"
  )

  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FAOBJ", "FALNKGRP", "FATPT"))
})

## Test 3: Check if there are multiple records with maximum diameter within by group then
## the MAXDIAM record is identified correctly

testthat::test_that("derive_param_maxdiam Test 3: Check if there are multiple records with
          maximum diameter within by group then the MAXDIAM record is identified
          correctly", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~FATESTCD, ~FASCAT, ~ATPTN, ~ADT, ~FALNKGRP,
    "ABC001", "Swelling", "VAC1", 2, "DIAM", "Admin site", 3, "2015-02-10", "Swelling-VAC1",
    "ABC001", "Swelling", "VAC1", 2, "DIAM", "Admin site", 4, "2015-02-11", "Swelling-VAC1",
    "ABC001", "Swelling", "VAC2", 7, "DIAM", "Admin site", 3, "2015-02-10", "Swelling-VAC2",
    "ABC001", "Swelling", "VAC2", 8, "DIAM", "Admin site", 4, "2015-02-11", "Swelling-VAC2",
    "ABC001", "Swelling", "VAC2", 9, "DIAM", "Admin site", 5, "2015-02-10", "Swelling-VAC2",
    "ABC001", "Swelling", "VAC2", 9, "DIAM", "Admin site", 6, "2015-02-11", "Swelling-VAC2"
  )


  temp <- input %>%
    group_by(USUBJID, FAOBJ, FASCAT, ATPTREF, FALNKGRP) %>%
    filter(FAOBJ %in% c("Swelling") & FATESTCD == "DIAM") %>%
    arrange(desc(AVAL), .by_group = TRUE) %>%
    summarise(AVAL = first(AVAL)) %>%
    mutate(
      FATEST = "Maximum Diameter",
      FATESTCD = "MAXDIAM",
      DTYPE = "MAXIMUM"
    )
  expected <- bind_rows(input, temp)


  actual <- derive_param_maxdiam(
    dataset = input,
    filter = FAOBJ %in% c("REDNESS", "Swelling") & FATESTCD == "DIAM",
    by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
    test_maxdiam = "Maximum Diameter",
    testcd_maxdiam = "MAXDIAM"
  )

  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FAOBJ", "FALNKGRP", "ATPTN"))
})
