library(tibble)
library(tidyverse)
# suppdm
suppdm <- tribble(
  ~STUDYID,
  "ABC"
)

vx_suppdm <- suppdm %>%
  mutate(
    STUDYID = "ABC",
    USUBJID = "ABC-1001",
    RDOMAIN = "DM",
    IDVAR = "",
    IDVARVAL = "",
    QNAM = "RACIALD",
    QVAL = "OTHER",
    QLABEL = "Racial Designation",
    QORIG = "CRF"
  ) %>%
  add_row(
    STUDYID = "ABC",
    USUBJID = "ABC-1002",
    RDOMAIN = "DM",
    IDVAR = "",
    IDVARVAL = "",
    QNAM = "RACIALD",
    QVAL = "OTHER",
    QLABEL = "Racial Designation",
    QORIG = "CRF"
  )

dir <- tempdir()
save(vx_suppdm, file = file.path(dir, "vx_suppdm.rda"), compress = "bzip2")
