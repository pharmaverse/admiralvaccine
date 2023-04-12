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
library(admiraldev)
library(admiral)
library(admiralvaccine)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral vaccine mock sdtm data and adsl vaccine data

data("ex")
data("vs")
data("face")
data("adsl")

# Step1 - Merging supplementary datasets and FACE with EX

adface <- derive_vars_merged_vaccine(
  dataset = face,
  dataset_ex = ex,
  dataset_supp = NULL,
  dataset_suppex = NULL,
  ex_vars = exprs(EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC, VISIT, VISITNUM)
)

# Step2 - Basic Filter and Pre-processing for FACE

adface <- adface %>%
  filter(FACAT == "REACTOGENICITY" & grepl("ADMIN|SYS", FASCAT)) %>%
  convert_blanks_to_na() %>%
  mutate(
    FAOBJ = str_to_sentence(FAOBJ)
  )

# Step3 - Deriving Fever OCCUR records from VS if FAOBJ = "FEVER" records not
# present in FACE

adface <- derive_param_fever_occur(
  dataset = adface,
  source_data = vs,
  faobj = "Fever"
)

# Step4 - Creating ADT, ATM, ADTM

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

# Step5 - Creating the direct mapping variables (AVAL, AVALC, ATPTREF, AVISIT,
# AVISITN,ATPT,ATPTN)

adface <- adface %>%
  mutate(
    AVAL = FAORRES,
    AVAL = as.numeric(AVAL),
    AVALC = as.character(FAORRES),
    ATPTREF = FATPTREF,
    ATPT = FATPT,
    ATPTN = FATPTNUM
  )

# Step6 - Creating severity records from Diameter for Redness,Swelling,etc

adface <- derive_param_diam_to_sev(
  dataset = adface,
  filter_diam = "DIAMETER",
  filter_faobj = c("Redness", "Swelling"),
  testcd_sev = "SEV",
  test_sev = "Severity/Intensity",
  none = c(0, 2),
  mild = c(2, 5),
  mod = c(5, 10),
  sev = c(10)
)


# Step7 - Deriving Maximum Severity for Local and Systemic events

adface <- derive_param_maxsev(
  dataset = adface,
  exclude_events = NULL,
  filter_sev = "SEV",
  test_maxsev = "Maximum Severity",
  testcd_maxsev = "MAXSEV",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Step8 - Deriving Maximum Diameter for Administrative site reactions

adface <- derive_param_maxdiam(
  dataset = adface,
  filter = FAOBJ %in% c("Redness", "Erythema") & FATESTCD %in% c("DIAMETER", "LDIAM"),
  by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
  test_maxdiam = "Maximum Diameter",
  testcd_maxdiam = "MAXDIAM"
)

# Step9 - Deriving Maximum Temperature

adface <- derive_param_maxtemp(
  dataset = adface,
  filter_faobj = "Fever",
  test_maxtemp = "Maximum Temperature",
  testcd_maxtemp = "MAXTEMP",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Step 10 - Assigning PARAM, PARAMN, PARAMCD, PARCAT1 and PARCAT2 by Lookup table

library(tibble)
lookup_dataset <- tribble(
  ~FATESTCD,    ~PARAMCD,    ~FATEST,                ~FAOBJ,
  "SEV",        "SEVREDN",   "Severity",             "Redness",
  "DIAMETER",   "DIARE",     "Diameter",             "Redness",
  "MAXDIAM",    "MDIRE",     "Maximum Diameter cm",  "Redness",
  "MAXTEMP",    "MAXTEMP",   "Maximum Temperature",  "Fever",
  "OCCUR",      "OCFEVER",   "Occurrence Indicator", "Fever",
  "OCCUR",      "OCERYTH",   "Occurrence Indicator", "Erythema",
  "SEV",        "SEVPAIN",   "Severity",             "Pain at Injection site",
  "OCCUR",      "OCPAIN",    "Occurrence Indicator", "Pain at Injection site",
  "OCCUR",      "OCSWEL",    "Occurrence Indicator", "Swelling"
)

adface <- derive_vars_params(
  dataset = adface,
  lookup_dataset = lookup_dataset
)

# Step11 - Maximum flag ANL01FL and ANL02FL

adface <- derive_vars_max_flag(
  dataset = adface,
  flag1 = "ANL01FL",
  flag2 = "ANL02FL"
)

# Step12 - Creating flag variables for occurred events

adface <- derive_vars_event_flag(
  dataset = adface,
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  aval_cutoff = 2.5,
  new_var1 = EVENTFL,
  new_var2 = EVENTDFL
)

# Creating APERIOD variables
period_ref <- create_period_dataset(
  dataset = adsl,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT, TRTA = TRTxxA)
)

adface <- derive_vars_joined(
  adface,
  dataset_add = period_ref,
  by_vars = exprs(STUDYID, USUBJID),
  filter_join = ADT >= APERSDT & ADT <= APEREDT
)

# Basic filter for ADSL
adsl <- adsl %>%
  convert_blanks_to_na() %>%
  filter(!is.na(USUBJID))

# Merging ADFACE with ADSL
adface <- derive_vars_merged(
  dataset = adface,
  dataset_add = adsl,
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
