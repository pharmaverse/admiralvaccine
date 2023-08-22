
library(tibble)
library(tidyverse)


# suppce
suppce <- tribble(
  ~STUDYID,
  "ABC"
)

vx_suppce <- suppce %>%
  mutate(
    STUDYID = "ABC",
    USUBJID = "ABC-1001",
    RDOMAIN = "CE",
    IDVAR = "CESEQ",
    IDVARVAL = "1",
    QNAM = "CEEVAL",
    QVAL = "STUDY SUBJECT",
    QLABEL = "Evaluator",
    QORIG = "ASSIGNED"
  ) %>%
  add_row(
    STUDYID = "ABC",
    USUBJID = "ABC-1001",
    RDOMAIN = "CE",
    IDVAR = "CESEQ",
    IDVARVAL = "2",
    QNAM = "CEEVAL",
    QVAL = "STUDY SUBJECT",
    QLABEL = "Evaluator",
    QORIG = "ASSIGNED"
  ) %>%
  add_row(
    STUDYID = "ABC",
    USUBJID = "ABC-1002",
    RDOMAIN = "CE",
    IDVAR = "CESEQ",
    IDVARVAL = "1",
    QNAM = "CEEVAL",
    QVAL = "STUDY SUBJECT",
    QLABEL = "Evaluator",
    QORIG = "ASSIGNED"
  ) %>%
  add_row(
    STUDYID = "ABC",
    USUBJID = "ABC-1002",
    RDOMAIN = "CE",
    IDVAR = "CESEQ",
    IDVARVAL = "2",
    QNAM = "CEEVAL",
    QVAL = "STUDY SUBJECT",
    QLABEL = "Evaluator",
    QORIG = "ASSIGNED"
  )
dir <- tempdir()
save(vx_suppce, file = file.path(dir, "vx_suppce.rda"), compress = "bzip2")
