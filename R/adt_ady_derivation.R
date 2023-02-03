library(lubridate)

# ADT and ADY derivation
# Tried to test this function with missing days and/or months:
# To put highest_imputation = "M" and date_imputation = "mid" to be in line with GSK rules.
# flag_imputation = "none" to suppress ADTF variable.

#test1_1 <- derive_vars_dt(
#  dataset = test1,
#  new_vars_prefix = "A",
#  dtc = ISDTC,
#  highest_imputation = "M",
#  date_imputation = "mid",
#  flag_imputation = "none"
#)

is2_adt <- derive_vars_dt(
  dataset = is_suppis,
  new_vars_prefix = "A",
  dtc = ISDTC
  )

# Merge to DM/ADSL to get RFSTDTC info in order to derive ADY
is2_rf <- left_join(is2_adt, dm, by = c("STUDYID","USUBJID")) %>%
  mutate(ADT = as.Date(ADT),
         RFSTDTC = as.Date(RFSTDTC)
         )

# Not standard for GSK
is2_ady <- derive_vars_dy(
  dataset = is2_rf,
  reference_date = RFSTDTC,
  source_vars = vars(ADT)
  )
