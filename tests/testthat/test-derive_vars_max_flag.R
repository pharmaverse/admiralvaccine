## Test 1: Derive maximum severity flag variables

test_that("derive_vars_max_flag Test 1: Derive maximum severity flag variables", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT"
  )

  expected1 <- input %>%
    arrange(desc(AVAL), FATPT) %>%
    group_by(USUBJID, FAOBJ, FATPTREF) %>%
    mutate(temp = row_number()) %>%
    mutate(flag1 = if_else(temp == 1, "Y", NA_character_)) %>%
    arrange(FAOBJ, FATESTCD) %>%
    select(-temp)

  expected2 <- input %>%
    arrange(desc(AVAL), FATPT) %>%
    group_by(USUBJID, FAOBJ) %>%
    mutate(temp = row_number()) %>%
    mutate(flag2 = if_else(temp == 1, "Y", NA_character_)) %>%
    arrange(FAOBJ, FATESTCD) %>%
    ungroup() %>%
    select(-temp)

  expected_output <- merge(expected1, expected2) %>%
    rename(ANL01FL = flag1, ANL02FL = flag2)

  actual_output <- derive_vars_max_flag(
    dataset = input,
    flag1 = "ANL01FL",
    flag2 = "ANL02FL"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "FATPT",
      "FATESTCD", "FATPTREF", "ANL01FL", "ANL02FL"
    )
  )
})

## Test 2: Derive maximum severity flag variable per event

test_that("derive_vars_max_flag Test 2: Derive maximum severity flag variable per event", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT"
  )

  expected_output <- input %>%
    arrange(desc(AVAL), FATPT) %>%
    group_by(USUBJID, FAOBJ, FATPTREF) %>%
    mutate(temp = row_number()) %>%
    mutate(flag1 = if_else(temp == 1, "Y", NA_character_)) %>%
    arrange(FAOBJ, FATESTCD) %>%
    select(-temp) %>%
    rename(ANL01FL = flag1)


  actual_output <- derive_vars_max_flag(
    dataset = input,
    flag1 = "ANL01FL",
    flag2 = NULL
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "FATPT",
      "FATESTCD", "FATPTREF"
    )
  )
})

## Test 3: Check if the records with AVAL = NA are not flagged

test_that("derive_vars_max_flag Test 3: check if the records with AVAL = NA are not flagged", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT",
    "ABC101", "REDNESS", "OCCUR", "VACC 1", NA, "DAY 1", "LOCISR",
    "ABC101", "REDNESS", "OCCUR", "VACC 1", NA, "DAY 2", "LOCISR",
    "ABC101", "REDNESS", "OCCUR", "VACC 2", NA, "DAY 1", "LOCISR",
    "ABC101", "REDNESS", "OCCUR", "VACC 2", NA, "DAY 2", "LOCISR"
  )

  input <- input %>% filter(!is.na(PARAMCD) & FATESTCD != "OCCUR")

  expected_output <- input %>%
    arrange(desc(AVAL), FATPT) %>%
    group_by(USUBJID, FAOBJ) %>%
    mutate(temp = row_number()) %>%
    mutate(ANL02FL = if_else(temp == 1, "Y", NA_character_)) %>%
    arrange(FAOBJ, FATESTCD) %>%
    ungroup() %>%
    select(-temp)


  actual_output <- derive_vars_max_flag(
    dataset = input,
    flag1 = NULL,
    flag2 = "ANL02FL"
  )

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "FATPT",
      "FATESTCD", "FATPTREF"
    )
  )
})

## Test 4: Check for error if both flags are NULL

test_that("derive_vars_max_flag Test 4: Check for error if both flags are NULL", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT"
  )

  testthat::expect_error(
    derive_vars_max_flag(
      dataset = input,
      flag1 = NULL,
      flag2 = NULL
    ),
    regexp = paste("Both flag names cannot be NULL")
  )
})
