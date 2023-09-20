# creating suppis
library(tibble)
library(dplyr)
vx_suppis <- tribble(
  ~USUBJID, ~IDVAR, ~IDVARVAL, ~QNAM, ~QLABEL, ~QVAL, ~QORIG, ~QEVAL,
  "ABC-1001", "ISSEQ", "1", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1001", "ISSEQ", "2", "LOD", "Limit of Detection", "5", "CRF", NA,
  "ABC-1001", "ISSEQ", "3", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1001", "ISSEQ", "4", "LOD", "Limit of Detection", "8", "CRF", NA,
  "ABC-1001", "ISSEQ", "5", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1001", "ISSEQ", "6", "LOD", "Limit of Detection", "5", "CRF", NA,
  "ABC-1001", "ISSEQ", "7", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1001", "ISSEQ", "8", "LOD", "Limit of Detection", "8", "CRF", NA,
  "ABC-1002", "ISSEQ", "1", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1002", "ISSEQ", "2", "LOD", "Limit of Detection", "5", "CRF", NA,
  "ABC-1002", "ISSEQ", "3", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1002", "ISSEQ", "4", "LOD", "Limit of Detection", "8", "CRF", NA,
  "ABC-1002", "ISSEQ", "5", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1002", "ISSEQ", "6", "LOD", "Limit of Detection", "5", "CRF", NA,
  "ABC-1002", "ISSEQ", "7", "LOD", "Limit of Detection", "4", "CRF", NA,
  "ABC-1002", "ISSEQ", "8", "LOD", "Limit of Detection", "8", "CRF", NA
) %>%
  mutate(
    STUDYID = "ABC",
    RDOMAIN = "IS"
  ) %>%
  select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL, QORIG, QEVAL)

dir <- tempdir()
save(vx_suppis, file = file.path(dir, "vx_suppis.rda"), compress = "bzip2")
