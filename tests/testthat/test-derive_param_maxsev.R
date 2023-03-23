# test_derive_param_maxsev
library(rlang)
library(dplyr)
library(tibble)
# test case 1 -------------------------------------------------------------

# testthat
testthat::test_that("derive_param_maxsev Test 1: Check whether`AVAL`is derived
                    when AVAL is NA", {
  # input data
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FASCAT,
    "XYZ1001", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 3, "SEVERE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "CHILLS", NA, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", NA, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", NA, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", NA, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", NA, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", NA, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC"
  )


  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )
  }

  expected1 <- input %>%
    mutate(AVAL = format_aval(AVALC)) %>%
    arrange(USUBJID, FATESTCD, FATEST, FASCAT, FAOBJ, ATPTREF, AVAL) %>%
    group_by(USUBJID, FASCAT, FATESTCD, FATEST, FAOBJ, ATPTREF) %>%
    slice_tail(n = 1) %>%
    filter(FATESTCD == "SEV" & FAOBJ != "REDNESS") %>%
    mutate(DTYPE = "MAXIMUM", FATESTCD = "MAXSEV", FATEST = "Maximum severity")

  # expected dataset
  expected <- bind_rows(input %>% mutate(AVAL = format_aval(AVALC)), expected1)


  # actual dataset
  actual <- derive_param_maxsev(
    dataset = input,
    filter_sev = "SEV",
    exclude_events = "REDNESS",
    test_maxsev = "Maximum severity",
    testcd_maxsev = "MAXSEV",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
  )
  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "FASCAT", "FATESTCD", "FATEST", "FAOBJ",
      "ATPTREF", "AVALC", "AVAL"
    )
  )
})



# test case 2 -------------------------------------------------------------

testthat::test_that("derive_param_maxsev Test 2: Check whether`AVAL`is derived
                    eventhough AVAL is populated", {
  # input data
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FASCAT,
    "XYZ1001", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 3, "SEVERE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "CHILLS", 1, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", 2, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", 3, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 4, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 6, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 7, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC"
  )

  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )}
  expected1 <- input %>%
    mutate(AVAL = format_aval(AVALC)) %>%
    arrange(USUBJID, FATESTCD, FATEST, FASCAT, FAOBJ, ATPTREF, AVAL) %>%
    group_by(USUBJID, FASCAT, FATESTCD, FATEST, FAOBJ, ATPTREF) %>%
    slice_tail(n = 1) %>%
    filter(FATESTCD == "SEV" & FAOBJ != "REDNESS") %>%
    mutate(DTYPE = "MAXIMUM", FATESTCD = "MAXSEV", FATEST = "Maximum severity")
  # expected dataset
  expected <- bind_rows(input %>% mutate(AVAL = format_aval(AVALC)), expected1)


  # actual dataset
  actual <- derive_param_maxsev(
    dataset = input,
    filter_sev = "SEV",
    exclude_events = "REDNESS",
    test_maxsev = "Maximum severity",
    testcd_maxsev = "MAXSEV",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
  )
  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "FASCAT", "FATESTCD", "FATEST", "FAOBJ",
      "ATPTREF", "AVALC", "AVAL"
    )
  )
})


# test case 3 -------------------------------------------------------------

testthat::test_that("derive_param_maxsev Test 3: Checking whether the
`exclude_events` argument excluding the events which is passed", {
  # input data
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FASCAT,
    "XYZ1001", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 3, "SEVERE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "CHILLS", 1, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", 2, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", 3, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 4, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 6, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 7, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC"
  )
  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )}

  expected1 <- input %>%
    mutate(AVAL = format_aval(AVALC)) %>%
    arrange(USUBJID, FATESTCD, FATEST, FASCAT, FAOBJ, ATPTREF, AVAL) %>%
    group_by(USUBJID, FASCAT, FATESTCD, FATEST, FAOBJ, ATPTREF) %>%
    slice_tail(n = 1) %>%
    filter(FATESTCD == "SEV" & FAOBJ != "CHILLS") %>%
    mutate(DTYPE = "MAXIMUM", FATESTCD = "MAXSEV", FATEST = "Maximum severity")
  # expected_dataset
  expected <- bind_rows(input %>% mutate(AVAL = format_aval(AVALC)), expected1)


  # actual_dataset
  actual <- derive_param_maxsev(
    dataset = input,
    filter_sev = "SEV",
    exclude_events = "CHILLS",
    test_maxsev = "Maximum severity",
    testcd_maxsev = "MAXSEV",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
  )

  # test_that
  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "FASCAT", "FATESTCD", "FATEST", "FAOBJ",
      "ATPTREF", "AVALC", "AVAL"
    )
  )
})

testthat::test_that("Check whether we get max sev for the SEV recoreds
                    when exclude event is null", {
  # input data
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FASCAT,
    "XYZ1001", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 3, "SEVERE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
    "XYZ1001", "CHILLS", 1, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", 2, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1001", "CHILLS", 3, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 4, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 6, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
    "XYZ1002", "CHILLS", 7, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC"
  )
  format_aval <- function(x) {
    case_when(
      x == "NONE" ~ 0,
      x == "MILD" ~ 1,
      x == "MODERATE" ~ 2,
      x == "SEVERE" ~ 3
    )}

  expected1 <- input %>%
    mutate(AVAL = format_aval(AVALC)) %>%
    arrange(USUBJID, FATESTCD, FATEST, FASCAT, FAOBJ, ATPTREF, AVAL) %>%
    group_by(USUBJID, FASCAT, FATESTCD, FATEST, FAOBJ, ATPTREF) %>%
    slice_tail(n = 1) %>%
    filter(FATESTCD == "SEV") %>%
    mutate(DTYPE = "MAXIMUM", FATESTCD = "MAXSEV", FATEST = "Maximum severity")
  # expected_dataset
  expected <- bind_rows(input %>% mutate(AVAL = format_aval(AVALC)), expected1)


  # actual_dataset
  actual <- derive_param_maxsev(
    dataset = input,
    filter_sev = "SEV",
    exclude_events = NULL,
    test_maxsev = "Maximum severity",
    testcd_maxsev = "MAXSEV",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
  )

  # test_that
  expect_dfs_equal(actual,
                   expected,
                   keys = c(
                     "USUBJID", "FASCAT", "FATESTCD", "FATEST", "FAOBJ",
                     "ATPTREF", "AVALC", "AVAL"
                   )
  )
})

testthat::test_that("Check whether its throwing error when passing invalid Category in filter_sev", {
                      # input data
                      input <- tribble(
                        ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FASCAT,
                        "XYZ1001", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
                        "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
                        "XYZ1001", "REDNESS", 3, "SEVERE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
                        "XYZ1001", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
                        "XYZ1001", "CHILLS", 1, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
                        "XYZ1001", "CHILLS", 2, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
                        "XYZ1001", "CHILLS", 3, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
                        "XYZ1002", "CHILLS", 4, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
                        "XYZ1002", "CHILLS", 6, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
                        "XYZ1002", "CHILLS", 7, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC"
                      )

                      testthat::expect_error(
                        derive_param_maxsev(
                        dataset = input,
                        filter_sev = "SEVERE",
                        exclude_events = NULL,
                        test_maxsev = "Maximum severity",
                        testcd_maxsev = "MAXSEV",
                        by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
                      ),
                      regexp = paste0("SEVERE", " ", "doesn't exist in the FATESTCD"))

                    })

