## Test 1: Derive CRIT1 variables

test_that("derive_vars_crit Test 1: Derive CRIT1 variables", {
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
      CRIT1 = if_else(!is.na(CRIT1FL), "Titer >= ISLLOQ", as.character(NA)),
      CRIT1FN = if_else(CRIT1FL == "Y", 1, 0)
    )


  # actual dataset
  actual <- admiraldev::suppress_warning(
    derive_vars_crit(
      dataset = input,
      prefix = "CRIT1",
      crit_label = "Titer >= ISLLOQ",
      condition = !is.na(AVAL) & !is.na(ISLLOQ),
      criterion = rlang::expr(AVAL >= ISLLOQ)
    ),
    regexpr = "was deprecated"
  )

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ", "CRIT1FL",
      "CRIT1FN", "CRIT1"
    )
  )
})

## Test 2: Derive CRIT1 variables when AVAL is missing

test_that("derive_vars_crit Test 2: Derive CRIT1 variables when AVAL is missing", {
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
      CRIT1 = if_else(!is.na(CRIT1FL), "Titer >= ISLLOQ", as.character(NA)),
      CRIT1FN = if_else(CRIT1FL == "Y", 1, 0)
    )

  # actual dataset
  actual <- admiraldev::suppress_warning(
    derive_vars_crit(
      dataset = input,
      prefix = "CRIT1",
      crit_label = "Titer >= ISLLOQ",
      condition = !is.na(AVAL) & !is.na(ISLLOQ),
      criterion = rlang::expr(AVAL >= ISLLOQ)
    ),
    regexpr = "was deprecated"
  )

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ", "CRIT1FL",
      "CRIT1FN", "CRIT1"
    )
  )
})

## Test 3: Complicated selections and missing values for AVAL and ISLLOQ

test_that("derive_vars_crit Test 3: Complicated selections and missing values for AVAL and
          ISLLOQ", {
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
      CRIT1FL = if_else(
        AVAL >= ISLLOQ & AVAL >= 2 * BASE, "Y", "N"
      ),
      CRIT1 = if_else(!is.na(CRIT1FL), "Titer >= ISLLOQ and Titer >= 2*BASE", as.character(NA)),
      CRIT1FN = if_else(CRIT1FL == "Y", 1, 0)
    )

  # actual dataset
  actual <- admiraldev::suppress_warning(
    derive_vars_crit(
      dataset = input,
      prefix = "CRIT1",
      crit_label = "Titer >= ISLLOQ and Titer >= 2*BASE",
      condition = !is.na(AVAL),
      criterion = rlang::expr(AVAL >= ISLLOQ & AVAL >= 2 * BASE)
    ),
    regexpr = "was deprecated"
  )

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "AVAL", "ISLLOQ", "CRIT1FL",
      "CRIT1FN", "CRIT1"
    )
  )
})
