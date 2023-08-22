#suppex
library(tibble)
library(dplyr)
suppex <- tribble(
  ~STUDYID,
  "ABC"
)

vx_suppex <- suppex %>% mutate(STUDYID = "ABC",
                            USUBJID = "ABC-1001",
                            RDOMAIN = "EX",
                            IDVAR = "EXSEQ",
                            IDVARVAL = "1",
                            QNAM = "EXTDV",
                            QVAL = "N",
                            QLABEL = "Temporary Delay of Vaccination",
                            QORIG = "ASSIGNED") %>%
  add_row(STUDYID = "ABC",
          USUBJID = "ABC-1001",
          RDOMAIN = "EX",
          IDVAR = "EXSEQ",
          IDVARVAL = "2",
          QNAM = "EXTDV",
          QVAL = "Y",
          QLABEL = "Temporary Delay of Vaccination",
          QORIG = "ASSIGNED") %>%
  add_row(STUDYID = "ABC",
          USUBJID = "ABC-1002",
          RDOMAIN = "EX",
          IDVAR = "EXSEQ",
          IDVARVAL = "1",
          QNAM = "EXTDV",
          QVAL = "N",
          QLABEL = "Temporary Delay of Vaccination",
          QORIG = "ASSIGNED") %>%
  add_row(STUDYID = "ABC",
          USUBJID = "ABC-1002",
          RDOMAIN = "EX",
          IDVAR = "EXSEQ",
          IDVARVAL = "2",
          QNAM = "EXTDV",
          QVAL = "Y",
          QLABEL = "Temporary Delay of Vaccination",
          QORIG = "ASSIGNED")

dir <- tempdir()
save(vx_suppex, file = file.path(dir, "vx_suppex.rda"), compress = "bzip2")
