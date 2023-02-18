# Name: ADIS
#
# Label: Immunogenicity Analysis
#
# Input: is, suppis, adsl
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
install.packages("metatools", repos = "https://cloud.r-project.org")
library(metatools)
library(dplyr)
library(lubridate)


# Load source datasets ----
data("is")
data("suppis")
data("admiral_adsl")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint


is <- convert_blanks_to_na(is)
suppis <- convert_blanks_to_na(suppis)
adsl <- convert_blanks_to_na(admiral_adsl)


# Derivations ----

# STEP 1 - combine IS with SUPPIS.
# Please, upload MOCK data
is_suppis <- combine_supp(is, suppis)


# STEP 2 - Visits and timing vriables derivation.
is1 <- is_suppis %>%
  mutate(
    AVISITN = as.numeric(VISITNUM),
    AVISIT = case_when(
      VISITNUM == 10 ~ "Visit 1",
      VISITNUM == 20 ~ "Visit 2",
      VISITNUM == 30 ~ "Visit 3",
      VISITNUM == 40 ~ "Visit 4",
      is.na(VISITNUM) ~ NA_character_
    ),
    ATPTN = as.numeric(VISITNUM / 10),
    ATPT = case_when(
      VISITNUM == 10 ~ "Visit 1 (Day 1)",
      VISITNUM == 20 ~ "Visit 2 (Day #)",
      VISITNUM == 30 ~ "Visit 3 (Day #)",
      VISITNUM == 40 ~ "Visit 4 (Day #)",
      is.na(VISITNUM) ~ NA_character_
    ),
    ATPTREF = case_when(
      VISITNUM %in% c(10, 20) ~ "FIRST TREATMENT",
      VISITNUM %in% c(30, 40) ~ "SECOND TREATMENT",
      is.na(VISITNUM) ~ NA_character_
    )
  )


# STEP 3: ADT and ADY derivation

# Tried to test this function with missing days and/or months and missing dates:
# To put highest_imputation = "M" and date_imputation = "mid" to be in line with GSK rules.
# flag_imputation = "none" to suppress ADTF variable.

# ADT derivation
is2_adt <- derive_vars_dt(
  dataset = is1,
  new_vars_prefix = "A",
  dtc = ISDTC,
  highest_imputation = "M",
  date_imputation = "mid",
  flag_imputation = "none"
)


# Merge with ADSL to get RFSTDTC info in order to derive ADY
is2_rf <- derive_var_merged_character(
  dataset = is2_adt,
  dataset_add = adsl,
  by_vars = exprs(STUDYID, USUBJID),
  new_var = RFSTDTC,
  source_var = RFSTDTC
) %>%
  mutate(
    ADT = as.Date(ADT),
    RFSTDTC = as.Date(RFSTDTC)
  )


# ADY derivation
is2_ady <- derive_vars_dy(
  dataset = is2_rf,
  reference_date = RFSTDTC,
  source_vars = exprs(ADT)
)
