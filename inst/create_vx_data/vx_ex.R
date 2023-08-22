# creating EX vaccine data
library(tibble)
library(magrittr)

ex <- tribble(
  ~USUBJID, ~EXSEQ, ~EXDTC, ~EXSTDTC, ~EXENDTC, ~EXDY, ~VISITNUM,
  "ABC-1001", 1, "2021-11-03T10:50:00", "2021-11-03T10:50:00", "2021-11-03T10:50:00", 1, 1,
  "ABC-1001", 2, "2021-12-30T09:10:00", "2021-12-30T09:10:00", "2021-12-30T09:10:00", 58, 2,
  "ABC-1002", 1, "2021-10-07T12:48:00", "2021-10-07T12:48:00", "2021-10-07T12:48:00", 1, 1,
  "ABC-1002", 2, "2021-12-16T12:41:00", "2021-12-16T12:41:00", "2021-12-16T12:41:00", 71, 2
) %>%
  mutate(
    STUDYID = "ABC",
    DOMAIN = "EX",
    EXCAT = "INVESTIGATIONAL PRODUCT",
    EXDOSU = "SYRINGE",
    EXDOSE = 1,
    EXDOSFRM = "INJECTION",
    EXROUTE = "INTRAMUSCULAR",
    VISIT = case_when(
      VISITNUM == 1 ~ "VISIT 1",
      VISITNUM == 2 ~ "VISIT 2",
      TRUE ~ NA_character_
    ),
    EPOCH = ifelse(EXSEQ == 1, "VACCINATION 1", "VACCINATION 2"),
    EXLNKGRP = EPOCH,
    EXTRT = ifelse(EXSEQ == 1, "VACCINE A", "VACCINE B"),
    EXLOC = "DELTOID MUSCLE",
    EXLAT = "LEFT",
    EXLNKID = paste(EXLNKGRP, EXLOC, EXLAT, sep = "-")
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXSEQ, EXLNKGRP, EXLNKID, EXTRT, EXCAT, EXDOSE, EXDOSU, EXDOSFRM,
    EXROUTE, EXLOC, EXLAT, VISITNUM, VISIT, EPOCH, EXDTC, EXSTDTC, EXENDTC, EXDY
  )


vx_ex <- ex

dir <- tempdir()
save(vx_ex, file = file.path(dir, "vx_ex.rda"), compress = "bzip2")
