# creating IS vaccine dataset
library(tibble)
library(dplyr)
library(tidyr)
vx_is <- tibble::tribble(
  ~USUBJID, ~ISSEQ, ~ISTESTCD, ~ISORRES, ~ISORRESU, ~ISSTRESC, ~ISSTRESN, ~ISSTRESU, ~ISSTAT,
  ~ISREASND, ~ISBLFL, ~ISLLOQ, ~VISITNUM, ~EPOCH, ~ISDTC, ~ISDY, ~ISULOQ,
  "ABC-1001", 1L, "J0033VN", NA, NA, NA, NA, NA, "NOT DONE",
  "INVALID RESULT", "Y", 2, 10, "FIRST TREATMENT", NA, NA, 100,
  "ABC-1001", 2L, "I0019NT", "3", "1/DIL", "3", 3, "titer", NA,
  NA, "Y", 4, 10, "FIRST TREATMENT", "2021-11", "1", 200,
  "ABC-1001", 3L, "M0019LN", ">150", "1/DIL", ">150", NA, "titer", NA,
  NA, "Y", 8, 10, "FIRST TREATMENT", "2021-11", "1", 150,
  "ABC-1001", 4L, "R0003MA", "140.5", "1/DIL", "140.5", 140.5, "titer", NA,
  NA, "Y", 4, 10, "FIRST TREATMENT", "2021-11", "1", 120,
  "ABC-1001", 5L, "J0033VN", "2", "1/DIL", "2", 2, "titer", NA,
  NA, NA, 2, 30, "SECOND TREATMENT", "2021-12", "61", 100,
  "ABC-1001", 6L, "I0019NT", ">200", "1/DIL", ">200", NA, "titer", NA,
  NA, NA, 4, 30, "SECOND TREATMENT", "2021-12", "61", 200,
  "ABC-1001", 7L, "M0019LN", "<2", "1/DIL", "<2", NA, "titer", NA,
  NA, NA, 8, 30, "SECOND TREATMENT", "2021-12", "61", 150,
  "ABC-1001", 8L, "R0003MA", "98.2", "1/DIL", "98.2", 98.2, "titer", NA,
  NA, NA, 4, 30, "SECOND TREATMENT", "2021-12", "61", 120,
  "ABC-1002", 1L, "J0033VN", "3", "1/DIL", "3", 3, "titer", NA,
  NA, "Y", 2, 10, "FIRST TREATMENT", "2021", "1", 100,
  "ABC-1002", 2L, "I0019NT", NA, NA, NA, NA, NA, "NOT DONE",
  "INVALID RESULT", "Y", 4, 10, "FIRST TREATMENT", NA, NA, 200,
  "ABC-1002", 3L, "M0019LN", "<2", "1/DIL", "<2", NA, "titer", NA,
  NA, "Y", 8, 10, "FIRST TREATMENT", "2021", "1", 150,
  "ABC-1002", 4L, "R0003MA", "48.9", "1/DIL", "48.9", 48.9, "titer", NA,
  NA, "Y", 4, 10, "FIRST TREATMENT", "2021", "1", 120,
  "ABC-1002", 5L, "J0033VN", ">100", "1/DIL", ">100", NA, "titer", NA,
  NA, NA, 2, 30, "SECOND TREATMENT", "2021", "61", 100,
  "ABC-1002", 6L, "I0019NT", "<2", "1/DIL", "<2", NA, "titer", NA,
  NA, NA, 4, 30, "SECOND TREATMENT", "2021", "61", 200,
  "ABC-1002", 7L, "M0019LN", "5", "1/DIL", "5", 5, "titer", NA,
  NA, NA, 8, 30, "SECOND TREATMENT", "2021", "61", 150,
  "ABC-1002", 8L, "R0003MA", "228.1", "1/DIL", "228.1", 228.1, "titer", NA,
  NA, NA, 4, 30, "SECOND TREATMENT", "2021", "61", 120
) %>%
  mutate(
    STUDYID = "ABC",
    DOMAIN = "IS",
    ISTEST = paste(ISTESTCD, "Antibody"),
    ISCAT = "IMMUNOLOGY",
    ISNAM = "LABNAME",
    ISSPEC = "SERUM",
    ISMETHOD = "METHODNAME"
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, ISSEQ, ISTESTCD, ISTEST, ISCAT, ISORRES, ISORRESU, ISSTRESC, ISSTRESN,
    ISSTRESU, ISSTAT, ISREASND, ISNAM, ISSPEC, ISMETHOD, ISBLFL, ISLLOQ, VISITNUM, EPOCH, ISDTC,
    ISDY, ISULOQ
  )

dir <- tempdir()
save(vx_is, file = file.path(dir, "vx_is.rda"), compress = "bzip2")
