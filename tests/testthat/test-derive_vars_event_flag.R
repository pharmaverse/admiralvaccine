# testcase-1

testthat::test_that("testcase-1 : checking whether its handling the NA values
                    and cutoff value is working fine", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT, ~DTYPE,
    "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", .5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", NA_integer_, NA_character_, "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "REDNESS", "VAC2", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 1.5, "1.5", "Diameter", "DIAMETER", "ADMIN-SITE", "MAXIMUM",
    "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 2, NA_character_, "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED"
  )

  actual_output <- derive_vars_event_flag(
    dataset = input,
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    aval_cutoff = 2.0,
    new_var1 = EVENTL,
    new_var2 = EVENTDL
  )


  expected_output <- input %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    mutate(
      EVENTL = case_when(
        any(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")) ~ "Y",
        TRUE ~ "N"
      ),
      EVENTDL = case_when(
        !is.na(AVAL) & AVAL > 2.0 | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE") ~ "Y",
        TRUE ~ "N"
      ),
      EVENTDL = ifelse(is.na(AVAL) & is.na(AVALC), NA_character_, EVENTDL),
      EVENTDL = case_when(
        DTYPE == "MAXIMUM" ~ NA_character_,
        TRUE ~ EVENTDL
      )
    )
  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FATEST", "ATPTREF", "AVAL", "AVALC", "FAOBJ", "EVENTL", "EVENTDL")
  )
})

# testcase-2

testthat::test_that("test case - 2: Checking whether it's creating the user input
                    varibales name for both flags", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT, ~DTYPE,
    "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "REDNESS", "VAC2", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 1.5, "1.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED"
  )

  expected_output <- input %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    mutate(
      flag1 = case_when(
        any(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")) ~ "Y",
        TRUE ~ "N"
      ),
      flag2 = case_when(
        !is.na(AVAL) & AVAL > 2.0 | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE") ~ "Y",
        TRUE ~ "N"
      ),
      flag2 = case_when(
        DTYPE == "MAXIMUM" ~ NA_character_,
        TRUE ~ flag2
      )
    ) %>%
    rename(EFL = flag1, EDFL = flag2)


  actual_output <- derive_vars_event_flag(
    dataset = input,
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    aval_cutoff = 2.0,
    new_var1 = EFL,
    new_var2 = EDFL
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FATEST", "ATPTREF", "AVAL", "AVALC", "FAOBJ", "EFL", "EDFL")
  )
})

# testcase - 3

testthat::test_that("test case - 3: Checking whether its creating only first flag", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT, ~DTYPE,
    "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "REDNESS", "VAC2", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 1.5, "1.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED"
  )

  expected_output <- input %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    mutate(
      flag2 = case_when(
        DTYPE != "MAXIMUM" & !is.na(AVAL) & AVAL > 2.0 |
          AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE") ~ "Y",
        TRUE ~ "N"
      )
    ) %>%
    rename(EDFL = flag2)


  actual_output <- derive_vars_event_flag(
    dataset = input,
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    aval_cutoff = 2.0,
    new_var1 = NULL,
    new_var2 = EDFL
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FATEST", "ATPTREF", "AVAL", "AVALC", "FAOBJ", "EDFL")
  )
})

## test case - 4

testthat::test_that("test case - 4: Checking whether its creating only second flag", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT, ~DTYPE,
    "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "REDNESS", "VAC2", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 1.5, "1.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED"
  )

  expected_output <- input %>%
    group_by(USUBJID, FAOBJ, ATPTREF) %>%
    mutate(
      flag1 = case_when(
        any(!is.na(AVAL) & AVAL > 2.0 | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")) ~ "Y",
        TRUE ~ "N"
      )
    ) %>%
    rename(EFL = flag1)


  actual_output <- derive_vars_event_flag(
    dataset = input,
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    aval_cutoff = 2.0,
    new_var1 = EFL,
    new_var2 = NULL
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FATEST", "ATPTREF", "AVAL", "AVALC", "FAOBJ", "EFL")
  )
})

## test case - 5

testthat::test_that("test case - 5: Checking whether return the input dataset
                    when user pass null in both flags", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT, ~DTYPE,
    "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC1", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "REDNESS", "VAC2", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "REDNESS", "VAC2", 1.5, "1.5", "Diameter", "DIAMETER", "ADMIN-SITE", "DERIVED",
    "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "DERIVED",
    "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "DERIVED"
  )

  expected_output <- input

  actual_output <- derive_vars_event_flag(
    dataset = input,
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    aval_cutoff = 2.0,
    new_var1 = NULL,
    new_var2 = NULL
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c("USUBJID", "FATEST", "ATPTREF", "AVAL", "AVALC", "FAOBJ")
  )
})
