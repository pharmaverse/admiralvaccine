#suppface
library(tibble)
library(dplyr)
suppface <- tribble(
  ~STUDYID,
  "ABC"
)

vx_suppface <- suppface %>% mutate(STUDYID = "ABC",
                                USUBJID = "ABC-1001",
                                RDOMAIN = "FACE",
                                IDVAR = "FASEQ",
                                IDVARVAL = "1",
                                QNAM = "CLTYP",
                                QVAL = "DAIRY",
                                QLABEL = "Collection Type",
                                QORIG = "Predecessor") %>%
  add_row(STUDYID = "ABC",
          USUBJID = "ABC-1001",
          RDOMAIN = "FACE",
          IDVAR = "FASEQ",
          IDVARVAL = "2",
          QNAM = "CLTYP",
          QVAL = "DAIRY",
          QLABEL = "Collection Type",
          QORIG = "Predecessor") %>%
  add_row(STUDYID = "ABC",
          USUBJID = "ABC-1002",
          RDOMAIN = "FACE",
          IDVAR = "FASEQ",
          IDVARVAL = "1",
          QNAM = "CLTYP",
          QVAL = "DAIRY",
          QLABEL = "Collection Type",
          QORIG = "Predecessor") %>%
  add_row(STUDYID = "ABC",
          USUBJID = "ABC-1002",
          RDOMAIN = "FACE",
          IDVAR = "FASEQ",
          IDVARVAL = "2",
          QNAM = "CLTYP",
          QVAL = "DAIRY",
          QLABEL = "Collection Type",
          QORIG = "Predecessor")

dir <- tempdir()
save(vx_suppface, file = file.path(dir, "vx_suppface.rda"), compress = "bzip2")
