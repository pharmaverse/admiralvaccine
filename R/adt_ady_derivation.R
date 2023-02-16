library(lubridate)

# STEP 3: ADT and ADY derivation

# Tried to test this function with missing days and/or months and missing dates:
# To put highest_imputation = "M" and date_imputation = "mid" to be in line with GSK rules.
# flag_imputation = "none" to suppress ADTF variable.

#test1 <- derive_vars_dt(
#  dataset = is_suppis,
#  new_vars_prefix = "A",
#  dtc = ISDTC,
#  highest_imputation = "M",
#  date_imputation = "mid",
#  flag_imputation = "none"
#)


# ADT derivation
is2_adt <- derive_vars_dt(
  dataset = is_suppis,
  new_vars_prefix = "A",
  dtc = ISDTC,
  highest_imputation = "M",
  date_imputation = "mid",
  flag_imputation = "none"
  )


# Merge with ADSL to get RFSTDTC info in order to derive ADY
is2_rf <- derive_var_merged_character(dataset = is2_adt,
                                      dataset_add = adsl,
                                      by_vars = vars(STUDYID,USUBJID),
                                      new_var = RFSTDTC,
                                      source_var = RFSTDTC
                                      ) %>%
  mutate(ADT = as.Date(ADT),
         RFSTDTC = as.Date(RFSTDTC)
         )


# ADY derivation
is2_ady <- derive_vars_dy(
  dataset = is2_rf,
  reference_date = RFSTDTC,
  source_vars = vars(ADT)
  )