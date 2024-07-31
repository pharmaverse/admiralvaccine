# Name: ADSL VACCINE
#
# Label: Subject Level Analysis Dataset for Vaccine
#
# Input: dm, ex
library(admiral)
library(pharmaversesdtm)
library(admiralvaccine)
library(dplyr)
library(lubridate)
library(stringr)
# Load source datasets

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("dm_vaccine")
data("ex_vaccine")

dm <- dm_vaccine
ex <- ex_vaccine

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html#handling-of-missing-values # nolint


dm <- convert_blanks_to_na(dm)
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

# Derivations ----
# impute start and end time of exposure to first and last respectively, do not impute date

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN"
  )

adsl <- dm %>%
  ## derive treatment variables (TRT01P, TRT01A) ----
  # See also the "Visit and Period Variables" vignette
  # (https://pharmaverse.github.io/admiral/cran-release/articles/visits_periods.html#treatment_adsl)
  mutate(
    TRT01P = substring(ARM, 1, 9),
    TRT02P = substring(ARM, 11, 100)
  ) %>%
  derive_vars_merged(
    dataset_add = ex,
    filter_add = EXLNKGRP == "VACCINATION 1",
    new_vars = exprs(TRT01A = EXTRT),
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  derive_vars_merged(
    dataset_add = ex,
    filter_add = EXLNKGRP == "VACCINATION 2",
    new_vars = exprs(TRT02A = EXTRT),
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  ## derive treatment start date (TRTSDTM) ----
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "VACCINE"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  ## derive treatment end date (TRTEDTM) ----
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "VACCINE"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  ## Derive treatment end/start date TRTSDT/TRTEDT ----
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

adsl <- derive_var_merged_exist_flag(
  dataset = adsl,
  dataset_add = ex,
  by_vars = get_admiral_option("subject_keys"),
  new_var = SAFFL,
  condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "VACCINE")))
) %>%
  ## creating PPROTFL variable
  mutate(
    PPROTFL = "Y"
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

# Creating period variables (Study Specific)
if ("VAX02DT" %in% names(adsl)) {
  adsl <- adsl %>%
    mutate(
      AP01SDT = VAX01DT,
      AP01EDT = if_else(!is.na(VAX02DT), VAX02DT - 1, as.Date(RFPENDTC)),
      AP02SDT = if_else(!is.na(VAX02DT), VAX02DT, NA_Date_),
      AP02EDT = if_else(!is.na(AP02SDT), as.Date(RFPENDTC), NA_Date_)
    )
} else {
  adsl <- adsl %>%
    mutate(
      AP01SDT = VAX01DT,
      AP01EDT = RFPENDTC
    )
}

admiralvaccine_adsl <- adsl

# Save output ----

dir <- tools::R_user_dir("admiralvaccine_templates_data", which = "cache")
# Change to whichever directory you want to save the dataset in
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(admiralvaccine_adsl, file = file.path(dir, "adsl.rda"), compress = "bzip2")
