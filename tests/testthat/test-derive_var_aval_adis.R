## Test 1: Derive AVAL variable with original and LOG10 values. No rounding applied

test_that("derive_var_aval_adis Test 1: Derive AVAL variable
                    with original and LOG10 values. No rounding applied", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ, ~ISULOQ,
    "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
    "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4, 200,
    "ABC-1001", 10, "M0019LN", "M0019LN Antibody", ">150", NA, 8, 150,
    "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4, 120,
    "ABC-1001", 30, "J0033VN", "J0033VN Antibody", "2", 2.0, 2, 100,
    "ABC-1001", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
    "ABC-1001", 30, "M0019LN", "M0019LN Antibody", NA, NA, 8, 150,
    "ABC-1001", 30, "R0003MA", "R0003MA Antibody", "98.2", 98.2, 4, 120,
    "ABC-1001", 10, "J0033VNL", "LOG10 (J0033VN Antibody)", NA, NA, 2, 100,
    "ABC-1001", 10, "I0019NTL", "LOG10 (I0019NT Antibody)", "3", 3.0, 4, 200,
    "ABC-1001", 10, "M0019LNL", "LOG10 (M0019LN Antibody)", ">150", NA, 8, 150,
    "ABC-1001", 10, "R0003MAL", "LOG10 (R0003MA Antibody)", "140.5", 140.5, 4, 120,
    "ABC-1001", 30, "J0033VNL", "LOG10 (J0033VN Antibody)", "2", 2.0, 2, 100,
    "ABC-1001", 30, "I0019NTL", "LOG10 (I0019NT Antibody)", NA, NA, 4, 200,
    "ABC-1001", 30, "M0019LNL", "LOG10 (M0019LN Antibody)", NA, NA, 8, 150,
    "ABC-1001", 30, "R0003MAL", "LOG10 (R0003MA Antibody)", "98.2", 98.2, 4, 120
  )

  # expected dataset
  expected <- input %>%
    mutate(
      AVAL = case_when(
        # ISORRES values without > or <
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ
        ~ ISSTRESN,
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ ISULOQ,
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ
        ~ log10(ISSTRESN),
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ log10(ISULOQ),

        # ISORRES values with > or <
        !grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~ ISULOQ,
        grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~ log10(ISULOQ)
      )
    )


  # actual dataset
  actual_a <- input %>%
    filter(!grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = ISLLOQ / 2,
      middle_rule = ISSTRESN,
      upper_rule = ISULOQ
    )

  actual_b <- input %>%
    filter(grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = log10(ISLLOQ / 2),
      middle_rule = log10(ISSTRESN),
      upper_rule = log10(ISULOQ)
    )

  actual <- bind_rows(actual_a, actual_b)

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "PARAM", "AVAL", "ISSTRESN",
      "ISLLOQ", "ISULOQ"
    )
  )
})



## Test 2: Derive AVAL variable by applying different rules per PARAMETER. No rounding applied

test_that("derive_var_aval_adis Test 2: Derive AVAL variable by
                    applying different rules per PARAMETER. No rounding applied", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ, ~ISULOQ,
    "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
    "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4, 200,
    "ABC-1001", 10, "M0019LN", "M0019LN Antibody", ">150", NA, 8, 150,
    "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4, 120,
    "ABC-1001", 10, "J0033VNF", "4FOLD (J0033VN Antibody)", NA, NA, 2, 100,
    "ABC-1001", 10, "I0019NTF", "4FOLD (I0019NT Antibody)", "3", 3.0, 4, 200,
    "ABC-1001", 10, "M0019LNF", "4FOLD (M0019LN Antibody)", ">150", NA, 8, 150,
    "ABC-1001", 10, "R0003MAF", "4FOLD (R0003MA Antibody)", "140.5", 140.5, 4, 120
  )

  # expected dataset
  expected <- input %>%
    mutate(
      AVAL = case_when(
        # ISORRES values without > or <
        !grepl("4FOLD", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ / 2,
        !grepl("4FOLD", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ
        ~ ISSTRESN,
        !grepl("4FOLD", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ ISULOQ,
        grepl("4FOLD", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ,
        grepl("4FOLD", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ
        ~ ISSTRESN,
        grepl("4FOLD", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ ISULOQ,

        # ISORRES values with > or <
        !grepl("4FOLD", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ / 2,
        !grepl("4FOLD", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~ ISULOQ,
        grepl("4FOLD", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ,
        grepl("4FOLD", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~ ISULOQ
      )
    )


  # actual dataset
  actual_a <- input %>%
    filter(!grepl("4FOLD", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = ISLLOQ / 2,
      middle_rule = ISSTRESN,
      upper_rule = ISULOQ
    )

  actual_b <- input %>%
    filter(grepl("4FOLD", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = ISLLOQ,
      middle_rule = ISSTRESN,
      upper_rule = ISULOQ
    )

  actual <- bind_rows(actual_a, actual_b)

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "PARAM", "AVAL", "ISSTRESN",
      "ISLLOQ", "ISULOQ"
    )
  )
})




## Test 3: Derive AVAL variable without ISULOQ

test_that("derive_var_aval_adis Test 3: Derive AVAL variable without ISULOQ", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ,
    "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2,
    "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4,
    "ABC-1001", 10, "M0019LN", "M0019LN Antibody", "150", NA, 8,
    "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4,
    "ABC-1001", 30, "J0033VN", "J0033VN Antibody", "2", 2.0, 2,
    "ABC-1001", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4,
    "ABC-1001", 30, "M0019LN", "M0019LN Antibody", NA, NA, 8,
    "ABC-1001", 30, "R0003MA", "R0003MA Antibody", "98.2", 98.2, 4,
    "ABC-1001", 10, "J0033VNL", "LOG10 (J0033VN Antibody)", NA, NA, 2,
    "ABC-1001", 10, "I0019NTL", "LOG10 (I0019NT Antibody)", "3", 3.0, 4,
    "ABC-1001", 10, "M0019LNL", "LOG10 (M0019LN Antibody)", "150", NA, 8,
    "ABC-1001", 10, "R0003MAL", "LOG10 (R0003MA Antibody)", "140.5", 140.5, 4,
    "ABC-1001", 30, "J0033VNL", "LOG10 (J0033VN Antibody)", "2", 2.0, 2,
    "ABC-1001", 30, "I0019NTL", "LOG10 (I0019NT Antibody)", NA, NA, 4,
    "ABC-1001", 30, "M0019LNL", "LOG10 (M0019LN Antibody)", NA, NA, 8,
    "ABC-1001", 30, "R0003MAL", "LOG10 (R0003MA Antibody)", "98.2", 98.2, 4
  )

  # expected dataset
  expected <- input %>%
    mutate(
      AVAL = case_when(
        # ISORRES values without > or <
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ ~ ISSTRESN,
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ ~ log10(ISSTRESN),

        # ISORRES values with > or <
        !grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~
          as.numeric(gsub("^.*?>", "", ISORRES)),
        grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~
          log10(as.numeric(gsub("^.*?>", "", ISORRES)))
      )
    )


  # actual dataset
  actual_a <- input %>%
    filter(!grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = ISLLOQ / 2,
      middle_rule = ISSTRESN
    )

  actual_b <- input %>%
    filter(grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = log10(ISLLOQ / 2),
      middle_rule = log10(ISSTRESN)
    )

  actual <- bind_rows(actual_a, actual_b)

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "PARAM", "AVAL", "ISSTRESN",
      "ISLLOQ"
    )
  )
})




## Test 4: Derive AVAL variable by applying rounding

test_that("derive_var_aval_adis Test 4: Derive AVAL variable
                    by applying rounding", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ, ~ISULOQ,
    "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
    "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4, 200,
    "ABC-1001", 10, "M0019LN", "M0019LN Antibody", ">150", NA, 8, 150,
    "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4, 120,
    "ABC-1001", 30, "J0033VN", "J0033VN Antibody", "2", 2.0, 2, 100,
    "ABC-1001", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
    "ABC-1001", 30, "M0019LN", "M0019LN Antibody", NA, NA, 8, 150,
    "ABC-1001", 30, "R0003MA", "R0003MA Antibody", "98.2", 98.2, 4, 120,
    "ABC-1001", 10, "J0033VNL", "LOG10 (J0033VN Antibody)", NA, NA, 2, 100,
    "ABC-1001", 10, "I0019NTL", "LOG10 (I0019NT Antibody)", "3", 3.0, 4, 200,
    "ABC-1001", 10, "M0019LNL", "LOG10 (M0019LN Antibody)", ">150", NA, 8, 150,
    "ABC-1001", 10, "R0003MAL", "LOG10 (R0003MA Antibody)", "140.5", 140.5, 4, 120,
    "ABC-1001", 30, "J0033VNL", "LOG10 (J0033VN Antibody)", "2", 2.0, 2, 100,
    "ABC-1001", 30, "I0019NTL", "LOG10 (I0019NT Antibody)", NA, NA, 4, 200,
    "ABC-1001", 30, "M0019LNL", "LOG10 (M0019LN Antibody)", NA, NA, 8, 150,
    "ABC-1001", 30, "R0003MAL", "LOG10 (R0003MA Antibody)", "98.2", 98.2, 4, 120
  )

  # expected dataset
  expected <- input %>%
    mutate(
      AVAL = case_when(
        # ISORRES values without > or <
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ
        ~ ISSTRESN,
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ ISULOQ,
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ
        ~ log10(ISSTRESN),
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ log10(ISULOQ),

        # ISORRES values with > or <
        !grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~ ISULOQ,
        grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~ log10(ISULOQ)
      ),
      AVAL = round(AVAL, 2)
    )


  # actual dataset
  actual_a <- input %>%
    filter(!grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = ISLLOQ / 2,
      middle_rule = ISSTRESN,
      upper_rule = ISULOQ,
      round = 2
    )

  actual_b <- input %>%
    filter(grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = log10(ISLLOQ / 2),
      middle_rule = log10(ISSTRESN),
      upper_rule = log10(ISULOQ),
      round = 2
    )

  actual <- bind_rows(actual_a, actual_b)

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "PARAM", "AVAL", "ISSTRESN",
      "ISLLOQ", "ISULOQ"
    )
  )
})



## Test 5: Derive AVAL variable by applying rounding without ISULOQ

test_that("derive_var_aval_adis Test 5: Derive AVAL variable
                    by applying rounding without ISULOQ", {
  # input data
  input <- tibble::tribble(
    ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ,
    "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2,
    "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4,
    "ABC-1001", 10, "M0019LN", "M0019LN Antibody", "150", NA, 8,
    "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4,
    "ABC-1001", 30, "J0033VN", "J0033VN Antibody", "2", 2.0, 2,
    "ABC-1001", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4,
    "ABC-1001", 30, "M0019LN", "M0019LN Antibody", NA, NA, 8,
    "ABC-1001", 30, "R0003MA", "R0003MA Antibody", "98.2", 98.2, 4,
    "ABC-1001", 10, "J0033VNL", "LOG10 (J0033VN Antibody)", NA, NA, 2,
    "ABC-1001", 10, "I0019NTL", "LOG10 (I0019NT Antibody)", "3", 3.0, 4,
    "ABC-1001", 10, "M0019LNL", "LOG10 (M0019LN Antibody)", "150", NA, 8,
    "ABC-1001", 10, "R0003MAL", "LOG10 (R0003MA Antibody)", "140.5", 140.5, 4,
    "ABC-1001", 30, "J0033VNL", "LOG10 (J0033VN Antibody)", "2", 2.0, 2,
    "ABC-1001", 30, "I0019NTL", "LOG10 (I0019NT Antibody)", NA, NA, 4,
    "ABC-1001", 30, "M0019LNL", "LOG10 (M0019LN Antibody)", NA, NA, 8,
    "ABC-1001", 30, "R0003MAL", "LOG10 (R0003MA Antibody)", "98.2", 98.2, 4
  )

  # expected dataset
  expected <- input %>%
    mutate(
      AVAL = case_when(
        # ISORRES values without > or <
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ ~ ISSTRESN,
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ ~ log10(ISSTRESN),

        # ISORRES values with > or <
        !grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ / 2,
        !grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~
          as.numeric(gsub("^.*?>", "", ISORRES)),
        grepl("LOG", PARAM) & grepl("<", ISORRES) & !is.na(ISORRES) ~ log10(ISLLOQ / 2),
        grepl("LOG", PARAM) & grepl(">", ISORRES) & !is.na(ISORRES) ~
          log10(as.numeric(gsub("^.*?>", "", ISORRES)))
      ),
      AVAL = round(AVAL, 2)
    )


  # actual dataset
  actual_a <- input %>%
    filter(!grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = ISLLOQ / 2,
      middle_rule = ISSTRESN,
      round = 2
    )

  actual_b <- input %>%
    filter(grepl("LOG", PARAM)) %>%
    derive_var_aval_adis(
      lower_rule = log10(ISLLOQ / 2),
      middle_rule = log10(ISSTRESN),
      round = 2
    )

  actual <- bind_rows(actual_a, actual_b)

  expect_dfs_equal(actual,
    expected,
    keys = c(
      "USUBJID", "AVISITN", "PARAMCD", "PARAM", "AVAL", "ISSTRESN",
      "ISLLOQ"
    )
  )
})
