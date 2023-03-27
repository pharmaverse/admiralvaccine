# Name: ADSL
#
# Label: Subject Level Analysis Dataset for Vaccine
#
# Input: dm, ex

# Loading required packages and admiralvaccine utilities
library(haven)
library(tibble)
library(metatools)
library(readxl)
library(admiraldev)
library(admiral)
library(purrr)
library(tidyverse)

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html#handling-of-missing-values # nolint

# Loading SDTM datasets
data("face")
data("vs")
data("ex")
data("admiralvaccine_adsl")

face <- convert_blanks_to_na(face)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
adsl <- convert_blanks_to_na(admiralvaccine_adsl)

# Step-1: Basic filter for ADSL

adsl <- adsl %>%
  convert_blanks_to_na() %>%
  filter(!is.na(USUBJID))

# Step-2: Merging FACE and EX

adface <- derive_vars_merged_vaccine(
  dataset = face,
  dataset_ex = ex,
  dataset_supp = NULL,
  dataset_suppex = NULL,
  ex_vars = exprs(EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC, VISIT, VISITNUM)
)


# Step 3: Basic Filter and Pre-processing for ADFACE

adface <- adface %>%
  filter(FACAT == "REACTOGENICITY" & grepl("ADMIN|SYS", FASCAT)) %>%
  convert_blanks_to_na() %>%
  mutate(
    FAOBJ = str_to_sentence(FAOBJ)
  )

# Step-4 Deriving Fever records

adface <- derive_param_fever_occur(
  dataset = adface,
  source_data = vs,
  faobj = "Fever"
)


# Step 5: Creating ADT, ATM, ADTM

adface <- adface %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = FADTC
  ) %>%
  derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = FADTC
  )

# Step 6: Creating the direct mapping variables (AVAL, AVALC)

adface <- adface %>%
  mutate(
    AVAL = FAORRES,
    AVAL = as.numeric(AVAL),
    AVALC = as.character(FAORRES),
    ATPTREF = FATPTREF,
    ATPT = FATPT,
    ATPTN = FATPTNUM
  )

# Step 8: Creating severity records from Diameter for Redness and Swelling

adface <- derive_param_diam_to_sev(
  dataset = adface,
  filter_diam = c("DIAMETER", "LDIAM"),
  filter_faobj = c("Redness", "Swelling", "Erythema"),
  testcd_sev = "SEV",
  test_sev = "Severity/Intensity",
  none = c(0, 2),
  mild = c(2, 5),
  mod = c(5, 10),
  sev = c(10)
)

# Step 9: Deriving Maximum Severity

adface <- derive_param_maxsev(
  dataset = adface,
  exclude_events = NULL,
  filter_sev = "SEV",
  test_maxsev = "Maximum Severity",
  testcd_maxsev = "MAXSEV",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Step 10: Deriving Maximum Diameter

adface <- derive_param_maxdiam(
  dataset = adface,
  filter = FAOBJ %in% c("REDNESS", "SWELLING") & FATESTCD == "DIAMETER",
  by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
  test_maxdiam = "Maximum Diameter",
  testcd_maxdiam = "MAXDIAM"
)

# Step 11: Deriving Maximum Temperature

adface <- derive_param_maxtemp(
  dataset = adface,
  filter_faobj = "Fever",
  test_maxtemp = "Maximum Temperature",
  testcd_maxtemp = "MAXTEMP",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Step 12: Assigning PARAM, PARAMN, PARAMCD, PARCAT1 and PARCAT2 by Lookup table

lookup_dataset <-
  read_xlsx("C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Admiral_vaccine/A/
            MRdata/adfacevdf_lookup.xlsx") %>%
  mutate(
    FAOBJ = str_to_sentence(FAOBJ),
    FATESTCD = toupper(FATESTCD)
  )

adface <- derive_vars_params(
  dataset = adface,
  lookup_dataset = lookup_dataset
)

# Step 13: Maximum flag ANL01FL and ANL02FL

adface <- derive_vars_max_flag(
  dataset = adface,
  flag1 = "ANL01FL",
  flag2 = "ANL02FL"
)

# Step 14: Creating flag variables for an occured events

adface <- derive_vars_event_flag(
  dataset = adface,
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  aval_cutoff = 2.5,
  new_var1 = EVENTL,
  new_var2 = EVENTDL
)

# Step 15: Merging ADFACE with ADSL

adface <- derive_vars_merged(
  dataset = adface,
  dataset_add = adsl,
  by_vars = exprs(STUDYID, USUBJID)
)

# Save output
saveRDS(adface_final, file = file.path(dir, "adface.rds"), compress = "bzip2")
