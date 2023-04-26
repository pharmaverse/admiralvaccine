# test case 1 -------------------------------------------------------------
# testthat
testthat::test_that("derive_vars_crit Test 1: Derive CRIT1 variables", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~AVAL, ~ISLLOQ,
    "999999-000001", 10, "J0033VN", 2, 4,
    "999999-000001", 10, "I0019NT", 3, 6,
    "999999-000001", 10, "M0019LN", 4, 4,
    "999999-000001", 10, "R0003MA", 3, 6,
    "999999-000001", 30, "J0033VN", 60, 4,
    "999999-000001", 30, "I0019NT", 567, 6,
    "999999-000001", 30, "M0019LN", 659, 4,
    "999999-000001", 30, "R0003MA", 250, 6,
    "999999-000002", 10, "J0033VN", 2, 4,
    "999999-000002", 10, "I0019NT", 7, 6,
    "999999-000002", 10, "M0019LN", 5, 4,
    "999999-000002", 10, "R0003MA", 3, 6,
    "999999-000002", 30, "J0033VN", 55, 4,
    "999999-000002", 30, "I0019NT", 89, 6,
    "999999-000002", 30, "M0019LN", 990, 4,
    "999999-000002", 30, "R0003MA", 340, 6
  )

  # expected dataset
  expected <- input %>%
    mutate(
      CRIT1FL = case_when(
        !is.na(AVAL) & !is.na(ISLLOQ) & AVAL >= ISLLOQ ~ "Y",
        !is.na(AVAL) & !is.na(ISLLOQ) & AVAL < ISLLOQ ~ "N",
        TRUE ~ as.character(NA)
      ),
      CRIT1FN = if_else(CRIT1FL == "Y", 1, 0),
      CRIT1 = if_else(!is.na(CRIT1FL), "Titer >= ISLLOQ", as.character(NA))
    )


  # actual dataset
  actual <- derive_vars_crit(
    dataset = input,
    new_var = "CRIT1",
    label_var = "Titer >= ISLLOQ",
    condition = !is.na(AVAL) & !is.na(ISLLOQ),
    criterion = AVAL >= ISLLOQ
  )

  expect_dfs_equal(actual,
    expected,
    keys = c("USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ")
  )
})




# test case 2 -------------------------------------------------------------
# testthat
testthat::test_that("derive_vars_crit Test 2: Derive CRIT1 variables
                    when AVAL is missing", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~AVAL, ~ISLLOQ,
    "999999-000001", 10, "J0033VN", 2, 4,
    "999999-000001", 10, "I0019NT", 3, 6,
    "999999-000001", 10, "M0019LN", 4, 4,
    "999999-000001", 10, "R0003MA", 3, 6,
    "999999-000001", 30, "J0033VN", 60, 4,
    "999999-000001", 30, "I0019NT", 567, 6,
    "999999-000001", 30, "M0019LN", 659, 4,
    "999999-000001", 30, "R0003MA", 250, 6,
    "999999-000002", 10, "J0033VN", NA, 4,
    "999999-000002", 10, "I0019NT", NA, 6,
    "999999-000002", 10, "M0019LN", NA, 4,
    "999999-000002", 10, "R0003MA", NA, 6,
    "999999-000002", 30, "J0033VN", NA, 4,
    "999999-000002", 30, "I0019NT", NA, 6,
    "999999-000002", 30, "M0019LN", NA, 4,
    "999999-000002", 30, "R0003MA", NA, 6
  )

  # expected dataset
  expected <- input %>%
    mutate(
      CRIT1FL = case_when(
        !is.na(AVAL) & !is.na(ISLLOQ) & AVAL >= ISLLOQ ~ "Y",
        !is.na(AVAL) & !is.na(ISLLOQ) & AVAL < ISLLOQ ~ "N",
        TRUE ~ as.character(NA)
      ),
      CRIT1FN = if_else(CRIT1FL == "Y", 1, 0),
      CRIT1 = if_else(!is.na(CRIT1FL), "Titer >= ISLLOQ", as.character(NA))
    )


  # actual dataset
  actual <- derive_vars_crit(
    dataset = input,
    new_var = "CRIT1",
    label_var = "Titer >= ISLLOQ",
    condition = !is.na(AVAL) & !is.na(ISLLOQ),
    criterion = AVAL >= ISLLOQ
  )

  expect_dfs_equal(actual,
    expected,
    keys = c("USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ")
  )
})




# test case 3 -------------------------------------------------------------
# testthat
testthat::test_that("derive_vars_crit Test 3: Try to apply different vars name and
                    missing ISLLOQ or AVAL", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~AVAL, ~ISLLOQ,
    "999999-000001", 10, "J0033VN", NA, NA,
    "999999-000001", 10, "I0019NT", NA, 6,
    "999999-000001", 10, "M0019LN", NA, 4,
    "999999-000001", 10, "R0003MA", NA, 6,
    "999999-000001", 30, "J0033VN", NA, NA,
    "999999-000001", 30, "I0019NT", NA, 6,
    "999999-000001", 30, "M0019LN", NA, 4,
    "999999-000001", 30, "R0003MA", NA, 6,
    "999999-000002", 10, "J0033VN", 2, NA,
    "999999-000002", 10, "I0019NT", 7, 6,
    "999999-000002", 10, "M0019LN", 5, 4,
    "999999-000002", 10, "R0003MA", 3, 6,
    "999999-000002", 30, "J0033VN", 55, NA,
    "999999-000002", 30, "I0019NT", 89, 6,
    "999999-000002", 30, "M0019LN", 990, 4,
    "999999-000002", 30, "R0003MA", 340, 6
  )

  # expected dataset
  expected <- input %>%
    mutate(
      ANL01FL = case_when(
        !is.na(AVAL) & !is.na(ISLLOQ) & AVAL >= ISLLOQ ~ "Y",
        !is.na(AVAL) & !is.na(ISLLOQ) & AVAL < ISLLOQ ~ "N",
        TRUE ~ as.character(NA)
      ),
      ANL01FN = if_else(ANL01FL == "Y", 1, 0),
      ANL01 = if_else(!is.na(ANL01FL), "Titer >= ISLLOQ", as.character(NA))
    )


  # actual dataset
  actual <- derive_vars_crit(
    dataset = input,
    new_var = "ANL01",
    label_var = "Titer >= ISLLOQ",
    condition = !is.na(AVAL) & !is.na(ISLLOQ),
    criterion = AVAL >= ISLLOQ
  )

  expect_dfs_equal(actual,
    expected,
    keys = c("USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ")
  )
})




# test case 4 -------------------------------------------------------------
# testthat
testthat::test_that("derive_vars_crit Test 4: Complicated selections and missing values
                    for AVAL and ISLLOQ", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~AVAL, ~ISLLOQ, ~BASE,
    "999999-000001", 10, "J0033VN", NA, NA, NA,
    "999999-000001", 10, "I0019NT", NA, 6, NA,
    "999999-000001", 10, "M0019LN", NA, 4, NA,
    "999999-000001", 10, "R0003MA", NA, 6, NA,
    "999999-000001", 30, "J0033VN", NA, NA, NA,
    "999999-000001", 30, "I0019NT", NA, 6, NA,
    "999999-000001", 30, "M0019LN", NA, 4, NA,
    "999999-000001", 30, "R0003MA", NA, 6, NA,
    "999999-000002", 10, "J0033VN", 2, NA, 2,
    "999999-000002", 10, "I0019NT", 7, 6, 7,
    "999999-000002", 10, "M0019LN", 5, 4, 5,
    "999999-000002", 10, "R0003MA", 3, 6, 3,
    "999999-000002", 30, "J0033VN", 55, NA, 2,
    "999999-000002", 30, "I0019NT", 89, 6, 7,
    "999999-000002", 30, "M0019LN", 990, 4, 5,
    "999999-000002", 30, "R0003MA", 340, 6, 3
  )

  # expected dataset
  expected <- input %>%
    mutate(
      CRIT1FL = case_when(
        !is.na(AVAL) & !is.na(ISLLOQ) & is.na(BASE) & AVAL >= ISLLOQ & AVAL >= 2 * BASE ~ "Y",
        !is.na(AVAL) & !is.na(ISLLOQ) & is.na(BASE) & AVAL < ISLLOQ & AVAL >= 2 * BASE ~ "N",
        TRUE ~ as.character(NA)
      ),
      CRIT1FN = if_else(CRIT1FL == "Y", 1, 0),
      CRIT1 = if_else(!is.na(CRIT1FL), "Titer >= ISLLOQ", as.character(NA))
    )


  # actual dataset
  actual <- derive_vars_crit(
    dataset = input,
    new_var = "CRIT1",
    label_var = "Titer >= ISLLOQ and Titer >= 2*BASE",
    condition = !is.na(AVAL) & !is.na(ISLLOQ) & is.na(BASE),
    criterion = AVAL >= ISLLOQ & AVAL >= 2 * BASE
  )

  expect_dfs_equal(actual,
    expected,
    keys = c("USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ")
  )
})
