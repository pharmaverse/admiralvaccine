# Name: ADFACE
#
# Label: Reactogenicity Analysis Dataset
#
# Input: face, suppface, ex, suppex, vs and ADSL

# Loading required packages

library(tibble)
library(dplyr)
library(stringr)
library(admiral)
library(admiralvaccine)
library(pharmaversesdtm)


# Load source datasets

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral vaccine mock SDTM data and ADSL vaccine data

data("ex_vaccine")
data("vs_vaccine")
data("face_vaccine")
data("admiralvaccine_adsl")
data("suppex_vaccine")
data("suppface_vaccine")

# Missing character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/cran-release/articles/admiral.html#handling-of-missing-values # nolint

ex <- convert_blanks_to_na(ex_vaccine)
vs <- convert_blanks_to_na(vs_vaccine)
face <- convert_blanks_to_na(face_vaccine)
adsl <- convert_blanks_to_na(admiralvaccine_adsl)
suppface <- convert_blanks_to_na(suppface_vaccine)
suppex <- convert_blanks_to_na(suppex_vaccine)

# creating a user defined function for deriving AVAL from AVALC

sev_to_numeric <- function(x, y) {
  case_when(
    x == "NONE" ~ 0,
    x == "MILD" ~ 1,
    x == "MODERATE" ~ 2,
    x == "SEVERE" ~ 3,
    TRUE ~ y
  )
}

# Step 1 - Basic Filter and Pre-processing for FACE

face <- face %>%
  filter(FACAT == "REACTOGENICITY" & grepl("ADMIN|SYS", FASCAT)) %>%
  mutate(FAOBJ = str_to_upper(FAOBJ))

adsl_vars <- exprs(RFSTDTC, RFENDTC)

# Combine the parental datasets with their respective supp datasets (only if exist)
# User can use `combine_supp()` from {metatools} to combine the parental with supp dataset.

face <- metatools::combine_supp(face, suppface)
ex <- metatools::combine_supp(ex, suppex)

# Step 2 - Merging supplementary datasets and FACE with EX

adface <- derive_vars_merged_vaccine(
  dataset = face,
  dataset_ex = ex,
  by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
  by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
  ex_vars = exprs(EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC, VISIT, VISITNUM)
) %>%
  # Step 3 - Merge required ADSL variables needed for analysis.
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  # Step 4 - Deriving Fever OCCUR records from VS if FAOBJ = "FEVER" records not present in FACE
  derive_fever_records(
    dataset_source = ungroup(vs),
    filter_source = VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP",
    faobj = "FEVER"
  ) %>%
  # Step 5 - Creating ADT, ATM, ADTM, ADY
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = FADTC
  ) %>%
  derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = FADTC,
    highest_imputation = "n"
  ) %>%
  mutate(RFSTDTC = as.Date(RFSTDTC)) %>%
  derive_vars_dy(reference_date = RFSTDTC, source_vars = exprs(ADT))

# Step 6 - Creating APERIOD variables
period_ref <- create_period_dataset(
  dataset = adsl,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT, TRTA = TRTxxA, TRTP = TRTxxP)
)

adface <- derive_vars_joined(
  adface,
  dataset_add = period_ref,
  by_vars = get_admiral_option("subject_keys"),
  filter_join = ADT >= APERSDT & ADT <= APEREDT,
  join_type = "all"
) %>%
  # Step 7 - Creating the direct mapping variables (AVAL, AVALC, ATPTREF, AVISIT, AVISITN, ATPT,
  # ATPTN)
  mutate(
    AVALC = as.character(FASTRESC),
    AVAL = suppressWarnings(as.numeric(FASTRESN)),
    AVAL = sev_to_numeric(AVALC, AVAL),
    ATPTREF = FATPTREF,
    ATPT = FATPT,
    ATPTN = FATPTNUM
  ) %>%
  # Please, consider which assessment is needed for your analysis. If you want to prioritize
  # Instigator assessment, please proceed as follows. Otherwise, change FAEVAL order.
  derive_var_extreme_flag(
    by = exprs(STUDYID, USUBJID, FATPTREF, FAOBJ, FATESTCD, FATPTNUM),
    order = exprs(STUDYID, USUBJID, FATPTREF, FAOBJ, FATESTCD, FATPTNUM, FAEVAL),
    new_var = ANL01FL,
    mode = "first",
    true_value = "Y",
    false_value = NA_character_
  )

# Version 0.3.0: as per CBER requirement, Investigator assessment has been added into FACE,
# which is identified by "INVESTIGATOR" value, in FAEVAL.

# step 8 - Derive the severity records from the Diameter records for the redness and swelling.
adface <- adface %>% derive_diam_to_sev_records(
  filter_add = ANL01FL == "Y",
  diam_code = "DIAMETER",
  faobj_values = c("REDNESS", "SWELLING"),
  testcd_sev = "SEV",
  test_sev = "Severity/Intensity",
  none = 0,
  mild = 2,
  mod = 5,
  sev = 10
)

# Step 9 - Deriving Maximum Severity for Local and Systemic events
adface <- adface %>% derive_extreme_records(
  dataset_add = adface,
  filter = FATESTCD == "SEV" & ANL01FL == "Y",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  order = exprs(AVAL),
  check_type = "none",
  mode = "last",
  set_values_to = exprs(
    FATEST = "Maximum Severity",
    FATESTCD = "MAXSEV"
  )
) %>%
  # Step 10 - Deriving Maximum Diameter for Administrative Site Reactions
  derive_extreme_records(
    dataset_add = adface,
    filter = FAOBJ %in% c("REDNESS", "SWELLING") & FATESTCD == "DIAMETER" & ANL01FL == "Y",
    by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
    order = exprs(AVAL),
    check_type = "none",
    mode = "last",
    set_values_to = exprs(
      FATEST = "Maximum Diameter",
      FATESTCD = "MAXDIAM"
    )
  ) %>%
  # Step 11 - Deriving Maximum Temperature
  derive_extreme_records(
    dataset_add = adface,
    filter = FAOBJ == "FEVER" & ANL01FL == "Y",
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    order = exprs(VSSTRESN),
    check_type = "none",
    mode = "last",
    set_values_to = exprs(
      FATEST = "Maximum Temperature",
      FATESTCD = "MAXTEMP"
    )
  )
# Step 12 - Assigning PARAM, PARAMN, PARAMCD, PARCAT1 and PARCAT2 by Lookup table

lookup_dataset <- tribble(
  ~FATESTCD, ~PARAMCD, ~PARAMN, ~FATEST, ~FAOBJ,
  "SEV", "SEVREDN", 1, "Severity/Intensity", "REDNESS",
  "DIAMETER", "DIARE", 2, "Diameter", "REDNESS",
  "MAXDIAM", "MDIRE", 3, "Maximum Diameter", "REDNESS",
  "MAXTEMP", "MAXTEMP", 4, "Maximum Temperature", "FEVER",
  "OCCUR", "OCFEVER", 5, "Occurrence Indicator", "FEVER",
  "OCCUR", "OCERYTH", 6, "Occurrence Indicator", "ERYTHEMA",
  "MAXSEV", "MAXSWEL", 7, "Maximum Severity", "SWELLING",
  "MAXSEV", "MAXREDN", 8, "Maximum Severity", "REDNESS",
  "MAXSEV", "MAXSFAT", 9, "Maximum Severity", "FATIGUE",
  "MAXSEV", "MAXSHEA", 10, "Maximum Severity", "HEADACHE",
  "MAXSEV", "MSEVNWJP", 11, "Maximum Severity", "NEW OR WORSENED JOINT PAIN",
  "MAXSEV", "MSEVNWMP", 12, "Maximum Severity", "NEW OR WORSENED MUSCLE PAIN",
  "OCCUR", "OCISR", 13, "Occurrence Indicator", "REDNESS",
  "OCCUR", "OCINS", 14, "Occurrence Indicator", "SWELLING",
  "OCCUR", "OCPIS", 15, "Occurrence Indicator", "PAIN AT INJECTION SITE",
  "OCCUR", "OCFATIG", 16, "Occurrence Indicator", "FATIGUE",
  "OCCUR", "OCHEAD", 17, "Occurrence Indicator", "HEADACHE",
  "OCCUR", "OCCHILLS", 18, "Occurrence Indicator", "CHILLS",
  "OCCUR", "OCDIAR", 19, "Occurrence Indicator", "DIARRHEA",
  "OCCUR", "OCCNWJP", 20, "Occurrence Indicator", "NEW OR WORSENED JOINT PAIN",
  "OCCUR", "OCCNWMP", 21, "Occurrence Indicator", "NEW OR WORSENED MUSCLE PAIN",
  "SEV", "SEVSWEL", 22, "Severity/Intensity", "SWELLING",
  "SEV", "SEVPIS", 23, "Severity/Intensity", "PAIN AT INJECTION SITE",
  "SEV", "SEVFAT", 24, "Severity/Intensity", "FATIGUE",
  "SEV", "SEVHEAD", 25, "Severity/Intensity", "HEADACHE",
  "SEV", "SEVDIAR", 26, "Severity/Intensity", "DIARRHEA",
  "SEV", "SEVNWJP", 27, "Severity/Intensity", "NEW OR WORSENED JOINT PAIN",
  "SEV", "SEVNWMP", 28, "Severity/Intensity", "NEW OR WORSENED MUSCLE PAIN",
  "MAXDIAM", "MDISW", 29, "Maximum Diameter", "SWELLING",
  "MAXSEV", "MAXSPIS", 30, "Maximum Severity", "PAIN AT INJECTION SITE",
  "OCCUR", "OCCVOM", 31, "Occurrence Indicator", "VOMITING",
  "DIAMETER", "DIASWEL", 32, "Diameter", "SWELLING"
)

adface <- derive_vars_params(
  dataset = adface,
  lookup_dataset = lookup_dataset,
  merge_vars = exprs(PARAMCD, PARAMN)
) %>%
  # Step 13 - Maximum flag ANL01FL and ANL02FL
  derive_vars_max_flag(
    flag1 = "ANL02FL",
    flag2 = "ANL03FL"
  ) %>%
  # Step 14 - Creating flag variables for occurred events
  derive_vars_event_flag(
    by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
    aval_cutoff = 2.5,
    new_var1 = EVENTFL,
    new_var2 = EVENTDFL
  )

# Step 15 - Basic filter for ADSL
adsl <- adsl %>%
  filter(!is.na(USUBJID))

# Merging ADFACE with ADSL
adface <- derive_vars_merged(
  dataset = adface,
  dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
  by_vars = get_admiral_option("subject_keys")
) %>%
  # Step 16 post processing
  post_process_reacto(
    filter_dataset = FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") |
      (FATESTCD %in% c("OCCUR", "SEV") & FAOBJ %in% c("FEVER", "REDNESS", "SWELLING"))
  )


# Step 17 retaining the required variables.
keep_vars <- c(
  "STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU", "SEX", "SEXN", "RACE",
  "RACEN", "ARACE", "ARACEN", "SAFFL", "COMPLFL", "ARM", "ARMCD", "ACTARM", "ACTARMCD",
  "ARMNRS", "ACTARMUD", "DARM", "DARMCD", "DACTARM", "DACTARCD", "TRTSDT", "TRTSDTM",
  "TRTSTM", "TRTEDT", "TRTEDTM", "TRTETM", "SRCDOM", "SRCSEQ", "FATEST", "FAGRPID",
  "FALNKID", "FALNKGRP", "FATESTCD", "PARAMCD", "PARAM", "PARAMN", "FAOBJ", "FALLT",
  "FAPTCD", "FADECOD", "FABODSYS", "FABDSYCD", "PARCAT1", "PARCAT2", "AVALC", "AVAL",
  "AVALCAT1", "AVALCA1N", "FASTAT", "FAREASND", "FAEVAL", "AVISITN", "AVISIT", "EPOCH",
  "ADT", "ADTM", "FAEVINTX", "DTYPE", "FASTINT", "FAENINT", "ADY", "ATPT", "ATPTN",
  "ATPTREF", "ATPTREFN", "EXDOSE", "EXTRT", "EXDOSU", "EXSTDTC", "EXENDTC", "TRTA",
  "TRTAN", "TRTP", "TRTPN", "APERIOD", "APERIODC", "APERSDT", "APERSTM", "APERSDTM",
  "APEREDT", "APERETM", "APEREDTM", "APERDY", "FAORRES"
)

admiralvaccine_adface <- adface %>% select(
  any_of(keep_vars), starts_with("TRT0"), starts_with("VAX"),
  starts_with("EVE"), starts_with("ANL")
)

# Save output ----

dir <- tools::R_user_dir("admiralvaccine_templates_data", which = "cache")
# Change to whichever directory you want to save the dataset in
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(admiralvaccine_adface, file = file.path(dir, "adface.rda"), compress = "bzip2")
