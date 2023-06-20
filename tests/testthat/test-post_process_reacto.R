## Test 1: Remove the records from the FA varibales where we have modified SDTM records

test_that("post_process_reacto Test 1: Remove the records from the FA varibales where we
          have modified SDTM records", {
  input <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FALAT, ~FACAT, ~FASCAT, ~FATPT, ~FATESTCD, ~PARAMCD, ~AVAL,
    "ABC-1002", "FEVER", NA, "REACTO", "SYS", "DAY 1", "MAXTEMP", "MAXTEMP", 39.4,
    "ABC-1002", "VOMITING", NA, "REACTO", "SYS", "DAY 4", "MAXSEV", "MAXVOMIT", 3,
    "ABC-1002", "SWELLING", "LEFT", "REACTO", "ADMIN", "DAY 1", "MAXSEV", "MAXSWEL", 3,
    "ABC-1002", "REDNESS", "LEFT", "REACTO", "ADMIN", "DAY 2", "DIAMETER", "DIARE", 10.3,
    "ABC-1002", "FEVER", NA, "REACTO", "SYS", "DAY 3", "OCCUR", "OCCFEV", NA,
    "ABC-1003", "REDNESS", NA, "REACTO", "SYS", "DAY 1", "MAXTEMP", "MAXTEMP", 33.4,
    "ABC-1003", "CHILLS", NA, "REACTO", "SYS", "DAY 4", "MAXSEV", "MAXVOMIT", 2,
    "ABC-1003", "SWELLING", "LEFT", "REACTO", "ADMIN", "DAY 1", "MAXSEV", "MAXSWEL", 1,
    "ABC-1003", "ERYTHEMA", "LEFT", "REACTO", "ADMIN", "DAY 3", "DIAMETER", "DIARE", 2.3,
    "ABC-1003", "FEVER", NA, "REACTO", "SYS", "DAY 2", "OCCUR", "OCCFEV", NA
  )

  expected <- input %>%
    filter(FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") |
      (FATESTCD == "OCCUR" & FAOBJ == "FEVER")) %>%
    mutate(
      FATPT = NA_character_,
      FASCAT = NA_character_,
      FACAT = NA_character_,
      FATESTCD = NA_character_,
      FAOBJ = NA_character_,
      FALAT = NA_character_
    ) %>%
    bind_rows(
      input %>% filter(!(FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") |
        (FATESTCD == "OCCUR" & FAOBJ == "FEVER")))
    )

  actual <- post_process_reacto(
    dataset = input,
    filter_dataset = FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") |
      (FATESTCD == "OCCUR" & FAOBJ == "FEVER")
  )
  expect_dfs_equal(
    expected,
    actual,
    keys = c(
      "USUBJID", "FAOBJ", "AVAL", "FATPT",
      "FATESTCD", "PARAMCD", "FASCAT", "FACAT", "FALAT"
    )
  )
})
