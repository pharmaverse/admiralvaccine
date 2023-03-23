library(admiraldev)
library(admiral)
library(rlang)
library(tidyverse)
library(diffdf)
library(testthat)

# test1
test_that("derive maximum severity flag varibles",{
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FADTC, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10,  "2015-01-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "2015-01-11", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "2015-02-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "2015-02-11", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1,  "2015-01-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-11", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "2015-02-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "2015-02-11", "SEVFAT")

  expected1 <- input %>%
    arrange(desc(AVAL),FADTC) %>%
    group_by(USUBJID,FAOBJ,FATPTREF) %>%
    mutate(temp = row_number()) %>%
    mutate(flag1=if_else(temp == 1,"Y",NA_character_)) %>%
    arrange(FAOBJ,FATESTCD) %>%
    select(-(temp))

  expected2 <- input %>%
    arrange(desc(AVAL),FADTC) %>%
    group_by(USUBJID,FAOBJ) %>%
    mutate(temp = row_number()) %>%
    mutate(flag2=if_else(temp == 1,"Y",NA_character_)) %>%
    arrange(FAOBJ,FATESTCD) %>% ungroup() %>%
    select(-temp)

  expected_output <- merge(expected1,expected2) %>%
    rename(ANL01FL=flag1,ANL02FL=flag2)

  actual_output <-derive_vars_max_flag(
    dataset = input,
    flag1="ANL01FL",
    flag2 = "ANL02FL")

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c('USUBJID','FAOBJ','AVAL','FADTC',
             'FATESTCD','FATPTREF','ANL01FL','ANL02FL')
  )
})

# test2
test_that("derive maximum severity flag variable per event",{
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FADTC, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10,  "2015-01-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "2015-01-11", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "2015-02-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "2015-02-11", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1,  "2015-01-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-11", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "2015-02-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "2015-02-11", "SEVFAT")

  expected_output <- input %>%
    arrange(desc(AVAL),FADTC) %>%
    group_by(USUBJID,FAOBJ,FATPTREF) %>%
    mutate(temp = row_number()) %>%
    mutate(flag1=if_else(temp == 1,"Y",NA_character_)) %>%
    arrange(FAOBJ,FATESTCD) %>%
    select(-(temp)) %>% rename(ANL01FL=flag1)


  actual_output <-derive_vars_max_flag(
    dataset = input,
    flag1 ="ANL01FL",
    flag2 = NULL)

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c('USUBJID','FAOBJ','AVAL','FADTC',
             'FATESTCD','FATPTREF')
  )
})

# test3
test_that("check if the records with `FATEST`='OCCUR' are not included
in the dataset",{
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FADTC, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10,  "2015-01-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "2015-01-11", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "2015-02-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "2015-02-11", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1,  "2015-01-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-11", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "2015-02-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "2015-02-11", "SEVFAT",
    "ABC101", "REDNESS", "OCCUR", "VACC 1", NA,  "2015-01-10", "LOCISR",
    "ABC101", "REDNESS", "OCCUR", "VACC 1", NA, "2015-01-11", "LOCISR",
    "ABC101", "REDNESS", "OCCUR", "VACC 2", NA, "2015-02-10", "LOCISR",
    "ABC101", "REDNESS", "OCCUR", "VACC 2", NA, "2015-02-11", "LOCISR"
  )

  input <- input %>% filter(!is.na(PARAMCD) & FATESTCD != "OCCUR")

  expected_output <- input %>%
    arrange(desc(AVAL),FADTC) %>%
    group_by(USUBJID,FAOBJ) %>%
    mutate(temp = row_number()) %>%
    mutate(ANL02FL=if_else(temp == 1,"Y",NA_character_)) %>%
    arrange(FAOBJ,FATESTCD) %>% ungroup() %>%
    select(-(temp))


  actual_output <-derive_vars_max_flag(
    dataset = input,
    flag1= NULL,
    flag2 = "ANL02FL")

  expect_dfs_equal(
    expected_output,
    actual_output,
    keys = c('USUBJID','FAOBJ','AVAL','FADTC',
             'FATESTCD','FATPTREF')
  )
})


testthat::test_that("derive maximum severity flag varibles",{
  input <- tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FADTC, ~PARAMCD,
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10,  "2015-01-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "2015-01-11", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "2015-02-10", "DIARE",
    "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "2015-02-11", "DIARE",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1,  "2015-01-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-11", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "2015-02-10", "SEVFAT",
    "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "2015-02-11", "SEVFAT")

testthat::expect_error(
  derive_vars_max_flag(
    dataset = input,
    flag1=NULL,
    flag2 =NULL),
regexp = paste("Please mention flag name"))
})
