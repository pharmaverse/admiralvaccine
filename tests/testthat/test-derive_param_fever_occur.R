library(admiraldev)
library(tibble)
library(admiral)
library(tidyverse)



# Test case 1 -------------------------------------------------------------


face <- tribble(
  ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT,
  "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",
  "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",
  "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",
  "ABC101", "FATIQUE", "OCCUR", "REACTOGENICITY", "SYSTEMIC", "DAY 3"
)

vs <- tribble(
  ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
  "ABC101", "TEMP", "REACTOGENICITY", 38.3, "C", "DAY 1",
  "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 2",
  "ABC101", "TEMP", "REACTOGENICITY", 36, "C", "DAY 3",
  "ABC101", "TEMP", "REACTOGENICITY", 37, "C", "DAY 4",
  "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 5",
  "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 6",
  "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 7"
)

expected1 <- vs %>% mutate(FAOBJ='FEVER', FATESTCD='OCCUR', FACAT='REACTOGENICITY',
                           FASCAT='SYSTEMIC', FATEST='Occurrence Indicator',
                           FAORRES=ifelse(VSSTRESN>=38, 'Y', 'N'),
                           FASTRESC=ifelse(VSSTRESN>=38, 'Y', 'N'),
                           DTYPE='DERIVED') %>%
  rename(FATPT=VSTPT) %>% select(-VSCAT)

expected <- bind_rows(face,expected1)

# debugonce(derive_param_fever_occur)
actual <- derive_param_fever_occur(
  dataset = face,
  source_data = vs,
  faobj = "FEVER"
)


testthat::test_that('derive_param_fever_occur Test 1: how the actual dataset is generated if FAOBJ="FEVER", if the FEVER records are not in FACE',
                    expect_dfs_equal(actual, expected, keys = c('USUBJID','FAOBJ','FATESTCD','FATEST','FATPT'))
)



# Test Case 2 -------------------------------------------------------------


face <- tribble(
  ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT, ~FAORRES, ~FASTRESC,
  "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",NA, NA,
  "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",NA, NA,
  "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",NA, NA,
  "ABC101", "FEVER", "OCCUR", "REACTOGENICITY", "SYSTEMIC", "DAY 3",'Y', 'Y',
)

vs <- tribble(
  ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
  "ABC101", "TEMP", "REACTOGENICITY", 38.3, "C", "DAY 1",
  "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 2",
  "ABC101", "TEMP", "REACTOGENICITY", 36, "C", "DAY 3",
  "ABC101", "TEMP", "REACTOGENICITY", 37, "C", "DAY 4",
  "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 5",
  "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 6",
  "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 7"
)

expected <- NULL

debugonce(derive_param_fever_occur)
actual <- derive_param_fever_occur(
  dataset = face,
  source_data = vs,
  faobj = "FEVER"
)


testthat::test_that('derive_param_fever_occur Test 2: how the actual dataset is generated if FAOBJ="FEVER", if the FEVER records are  in FACE',
                    testthat::expect_equal(actual, expected)
)



# Test Case 3 -------------------------------------------------------------


face <- tribble(
  ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT, ~FAORRES, ~FASTRESC,
  "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",NA_character_,NA_character_,
  "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",NA_character_,NA_character_,
  "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",NA_character_,NA_character_
)

vs <- tribble(
  ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
  "ABC101", "SBP", "REACTOGENICITY", 38.3, "C", "DAY 1",
  "ABC101", "SBP", "REACTOGENICITY", 38, "C", "DAY 2",
  "ABC101", "SBP", "REACTOGENICITY", 36, "C", "DAY 3",
  "ABC101", "SBP", "REACTOGENICITY", 37, "C", "DAY 4",
  "ABC101", "DBP", "REACTOGENICITY", 39, "C", "DAY 5",
  "ABC101", "DBP", "REACTOGENICITY", 39, "C", "DAY 6",
  "ABC101", "DBP", "REACTOGENICITY", 38, "C", "DAY 7"
)

expected <- face %>% mutate(VSTESTCD=NA_character_,
                            VSSTRESN=NA_real_, VSSTRESU=NA_character_,
                            FATEST=NA_character_, DTYPE=NA_character_
)

# debugonce(derive_param_fever_occur)
actual <- derive_param_fever_occur(
  dataset = face,
  source_data = vs,
  faobj = "FEVER"
)


testthat::test_that('derive_param_fever_occur Test 3: how the actual dataset is generated if FAOBJ does not have "FEVER" records,
                    and also the VS domain does not have temp records ',
                    testthat::expect_equal(actual, expected)
)


# Test Case 4 -------------------------------------------------------------


face <- tribble(
  ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT, ~FAORRES, ~FASTRESC,
  "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",NA_character_,NA_character_,
  "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",NA_character_,NA_character_,
  "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",NA_character_,NA_character_
)

vs <- tribble(
  ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
  "ABC101", "SBP", "VITAL SIGNS", 38.3, "C", "DAY 1",
  "ABC101", "SBP", "VITAL SIGNS", 38, "C", "DAY 2",
  "ABC101", "SBP", "VITAL SIGNS", 36, "C", "DAY 3",
  "ABC101", "SBP", "VITAL SIGNS", 37, "C", "DAY 4",
  "ABC101", "DBP", "VITAL SIGNS", 39, "C", "DAY 5",
  "ABC101", "DBP", "VITAL SIGNS", 39, "C", "DAY 6",
  "ABC101", "DBP", "VITAL SIGNS", 38, "C", "DAY 7"
)

expected <- face %>% mutate(VSTESTCD=NA_character_,
                            VSSTRESN=NA_real_, VSSTRESU=NA_character_,
                            FATEST=NA_character_, DTYPE=NA_character_
)

# debugonce(derive_param_fever_occur)
actual <- derive_param_fever_occur(
  dataset = face,
  source_data = vs,
  faobj = "FEVER"
)


testthat::test_that('derive_param_fever_occur Test 4: how the actual dataset is generated if FAOBJ does not have "FEVER" records,
                    and also the VS domain does not have temp records and have VSCAT=VITAL SIGNS ',
                    testthat::expect_equal(actual, expected)
)
