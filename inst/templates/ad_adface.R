# Name: ADFACE
#
# Label: Reactogenicity Analysis Dataset
#
# Input: face, ex, vs

# Loading required packages and admiral vaccine utilities

library(tibble)
library(dplyr)
library(metatools)
library(readxl)
library(stringr)
library(admiraldev)
library(admiral)
library(admiralvaccine)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral vaccine mock sdtm data and adsl vaccine data

data("vx_ex")
data("vx_vs")
data("vx_face")
data("vx_adsl")
data("vx_suppdm")
data("vx_suppex")
data("vx_suppface")

ex <- vx_ex
vs <- vx_vs
face <- vx_face
adsl <- vx_adsl
suppface <- vx_suppface
suppex <- vx_suppex


# Step1 - Basic Filter and Pre-processing for FACE

face <- face %>%
  filter(FACAT == "REACTOGENICITY" & grepl("ADMIN|SYS", FASCAT)) %>%
  convert_blanks_to_na() %>%
  mutate(FAOBJ = str_to_upper(FAOBJ))

# Step2 - Merging supplementary datasets and FACE with EX

adface <- derive_vars_merged_vaccine(
  dataset = face,
  dataset_ex = ex,
  dataset_supp = suppface,
  dataset_suppex = suppex,
  by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
  by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
  ex_vars = exprs(EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC, VISIT, VISITNUM)
)

# Step3 - Merge required adsl variables needed for analysis.

adsl_vars <- exprs(RFSTDTC, RFENDTC)

adface <- derive_vars_merged(
  adface,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = exprs(STUDYID, USUBJID)
)

# Step3 - Deriving Fever OCCUR records from VS if FAOBJ = "FEVER" records not
# present in FACE

adface <- derive_param_fever_occur(
  dataset = adface,
  source_data = vs,
  source_filter = "VSCAT == 'REACTOGENICITY' & VSTESTCD == 'TEMP'",
  faobj = "FEVER"
)

# Step4 - Creating ADT, ATM, ADTM, ADY

adface <- adface %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = FADTC
  ) %>%
  derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = FADTC,
    highest_imputation = "n"
  )

adface <- adface %>%
  mutate(RFSTDTC = as.Date(RFSTDTC)) %>%
  derive_vars_dy(reference_date = RFSTDTC, source_vars = exprs(ADT))

# Step5 - Creating APERIOD variables
period_ref <- create_period_dataset(
  dataset = adsl,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT, TRTA = TRTxxA, TRTP = TRTxxP)
)

adface <- derive_vars_joined(
  adface,
  dataset_add = period_ref,
  by_vars = exprs(STUDYID, USUBJID),
  filter_join = ADT >= APERSDT & ADT <= APEREDT
)

# Step6 - Creating the direct mapping variables (AVAL, AVALC, ATPTREF, AVISIT,
# AVISITN,ATPT,ATPTN)

adface <- adface %>%
  mutate(
    AVAL = suppressWarnings(as.numeric(FASTRESN)),
    AVALC = as.character(FASTRESC),
    ATPTREF = FATPTREF,
    ATPT = FATPT,
    ATPTN = FATPTNUM
  )

# Step7 - Creating severity records from Diameter for Redness,Swelling,etc

adface <- derive_param_diam_to_sev(
  dataset = adface,
  filter_diam = "DIAMETER",
  filter_faobj = c("REDNESS", "SWELLING"),
  testcd_sev = "SEV",
  test_sev = "Severity/Intensity",
  none = c(0, 2),
  mild = c(2, 5),
  mod = c(5, 10),
  sev = c(10)
)


# Step8 - Deriving Maximum Severity for Local and Systemic events

adface <- derive_param_maxsev(
  dataset = adface,
  exclude_events = NULL,
  filter_sev = "SEV",
  test_maxsev = "Maximum Severity",
  testcd_maxsev = "MAXSEV",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Step9 - Deriving Maximum Diameter for Administrative site reactions

adface <- derive_param_maxdiam(
  dataset = adface,
  filter = FAOBJ %in% c("REDNESS", "SWELLING") & FATESTCD == "DIAMETER",
  by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
  test_maxdiam = "Maximum Diameter",
  testcd_maxdiam = "MAXDIAM"
)

# Step10 - Deriving Maximum Temperature

adface <- derive_param_maxtemp(
  dataset = adface,
  filter_faobj = "FEVER",
  test_maxtemp = "Maximum Temperature",
  testcd_maxtemp = "MAXTEMP",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Step 11 - Assigning PARAM, PARAMN, PARAMCD, PARCAT1 and PARCAT2 by Lookup table

lookup_dataset <- tribble(
  ~FATESTCD, ~PARAMCD, ~PARAMN, ~FATEST, ~FAOBJ,
  "SEV", "SEVREDN", 1, "Severity", "REDNESS",
  "DIAMETER", "DIARE", 2, "Diameter", "REDNESS",
  "MAXDIAM", "MDIRE", 3, "Maximum Diameter cm", "REDNESS",
  "MAXTEMP", "MAXTEMP", 4, "Maximum Temperature", "FEVER",
  "OCCUR", "OCFEVER", 5, "Occurrence Indicator", "FEVER",
  "OCCUR", "OCERYTH", 6, "Occurrence Indicator", "ERYTHEMA",
  "SEV", "SEVPAIN", 7, "Severity", "PAIN AT INJECTION SITE",
  "OCCUR", "OCPAIN", 8, "Occurrence Indicator", "PAIN AT INJECTION SITE",
  "OCCUR", "OCSWEL", 9, "Occurrence Indicator", "SWELLING",
  "MAXSEV", "MAXSWEL", 10, "Maximum Severity", "SWELLING",
  "MAXSEV", "MAXREDN", 11, "Maximum Severity", "REDNESS"
)

adface <- derive_vars_params(
  dataset = adface,
  lookup_dataset = lookup_dataset,
  merge_vars = exprs(PARAMCD, PARAMN)
)

# Step12 - Maximum flag ANL01FL and ANL02FL

adface <- derive_vars_max_flag(
  dataset = adface,
  flag1 = "ANL01FL",
  flag2 = "ANL02FL"
)

# Step13 - Creating flag variables for occurred events

adface <- derive_vars_event_flag(
  dataset = adface,
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  aval_cutoff = 2.5,
  new_var1 = EVENTFL,
  new_var2 = EVENTDFL
)

# Basic filter for ADSL
adsl <- adsl %>%
  convert_blanks_to_na() %>%
  filter(!is.na(USUBJID))

# Merging ADFACE with ADSL
adface <- derive_vars_merged(
  dataset = adface,
  dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
  by_vars = exprs(STUDYID, USUBJID)
)

keep_vars <- c(
  "STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU", "SEX", "SEXN", "RACE", "RACEN", "ARACE",
  "ARACEN", "SAFFL", "COMPLFL", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "ARMNRS",
  "ACTARMUD", "DARM", "DARMCD", "DACTARM", "DACTARCD", "TRTSDT", "TRTSDTM",
  "TRTSTM", "TRTEDT", "TRTEDTM", "TRTETM", "SRCDOM", "SRCSEQ", "FATEST",
  "FAGRPID", "FALNKID", "FALNKGRP", "FATESTCD", "PARAMCD", "PARAM", "PARAMN", "FAOBJ",
  "FALLT", "FAPTCD", "FADECOD", "FABODSYS", "FABDSYCD", "PARCAT1", "PARCAT2", "AVALC",
  "AVAL", "AVALCAT1", "AVALCA1N", "FASTAT", "FAREASND", "FAEVAL", "AVISITN", "AVISIT",
  "EPOCH", "ADT", "ADTM", "FAEVINTX", "DTYPE", "FASTINT", "FAENINT", "ADY", "ATPT", "ATPTN",
  "ATPTREF", "ATPTREFN", "EXDOSE", "EXTRT", "EXDOSU", "EXSTDTC", "EXENDTC",
  "TRTA", "TRTAN", "TRTP", "TRTPN", "APERIOD", "APERIODC",
  "APERSDT", "APERSTM", "APERSDTM", "APEREDT", "APERETM", "APEREDTM", "APERDY",
  "FAORRES"
)

adface <- adface %>% select(
  any_of(keep_vars), starts_with("TRT0"), starts_with("VAX"),
  starts_with("EVE"), starts_with("ANL")
)

# Save output ----

dir <- tempdir()
save(adface, file = file.path(dir, "adface.rda"), compress = "bzip2")
