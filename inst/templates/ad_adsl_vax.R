# Name: ADSL VACCINE
#
# Label: Subject Level Analysis Dataset for Vaccine
#
# Input: dm, ex,
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(admiraldev)
library(admiralvaccine)

# Load source datasets

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("dm")
data("ex")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html#handling-of-missing-values # nolint


dm <- convert_blanks_to_na(dm) %>% head(1)
ex <- convert_blanks_to_na(ex)

# User defined functions ----

# Here are some examples of how you can create your own functions that
#  operates on vectors, which can be used in `mutate`.

# Grouping
format_racegr1 <- function(x) {
  case_when(
    x == "WHITE" ~ "White",
    x != "WHITE" ~ "Non-white",
    TRUE ~ "Missing"
  )
}

format_agegr1 <- function(x) {
  case_when(
    x < 18 ~ "<18",
    between(x, 18, 64) ~ "18-64",
    x > 64 ~ ">64",
    TRUE ~ "Missing"
  )
}

format_region1 <- function(x) {
  case_when(
    x %in% c("CAN", "USA") ~ "NA",
    !is.na(x) ~ "RoW",
    TRUE ~ "Missing"
  )
}

format_lddthgr1 <- function(x) {
  case_when(
    x <= 30 ~ "<= 30",
    x > 30 ~ "> 30",
    TRUE ~ NA_character_
  )
}

# EOSSTT mapping
format_eosstt <- function(x) {
  case_when(
    x %in% c("COMPLETED") ~ "COMPLETED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    !is.na(x) ~ "DISCONTINUED",
    TRUE ~ "ONGOING"
  )
}

# Derivations ----
# impute start and end time of exposure to first and last respectively, do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- dm %>%
  ## derive treatment variables (TRT01P, TRT01A) ----
  # See also the "Visit and Period Variables" vignette
  # (https://pharmaverse.github.io/admiral/cran-release/articles/visits_periods.html#treatment_adsl)
  mutate(TRT01P = ARM, TRT01A = ACTARM) %>%
  ## derive treatment start date (TRTSDTM) ----
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "VACCINE"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  ## derive treatment end date (TRTEDTM) ----
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "VACCINE"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  ## Derive treatment end/start date TRTSDT/TRTEDT ----
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  ## derive treatment duration (TRTDURD) ----
  derive_var_trtdurd()


adsl <- derive_var_merged_exist_flag(
  dataset = adsl,
  dataset_add = ex,
  by_vars = exprs(STUDYID, USUBJID),
  new_var = SAFFL,
  condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
) %>%
  ## Groupings and others variables
  mutate(
    RACEGR1 = format_racegr1(RACE),
    AGEGR1 = format_agegr1(AGE),
    REGION1 = format_region1(COUNTRY),
    DOMAIN = NULL
  )

# Creating Vaccination date from EX (Vaccine specific)

adsl <- derive_vars_vaxdt(
  dataset = ex,
  dataset_adsl = adsl,
  by_vars = exprs(USUBJID, VISITNUM),
  order = exprs(USUBJID, VISITNUM, VISIT, EXSTDTC)
)


# Save output
dir <- tempdir()
save(adsl, file = file.path(dir, "adsl_vax.rda"), compress = "bzip2")
