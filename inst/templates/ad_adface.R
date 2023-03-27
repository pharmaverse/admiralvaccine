# To Create ADFACE

# Loading required packages and admiralvaccine utilities
library(haven)
library(tibble)
library(metatools)
library(readxl)
library(admiraldev)
library(admiral)
library(purrr)
library(tidyverse)

util_path <-
  c("C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/NEW_R/")
file.source <- list.files(util_path, pattern = ".R$")
map(paste0(util_path, file.source), source)

# Loading SDTM datasets
load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/face.rda')
load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/vs.rda')
load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/ex.rda')
# load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/suppex.rda')
# load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/suppface.rda')
# load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/suppvs.rda')
load('C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Adface_ADmiral/mock data/adsl_vax.rds')

# Basic filter for ADSL

adsl <- adsl%>%  convert_blanks_to_na() %>%
  filter(!is.na(USUBJID))

# Merging FACE and EX

adface <- derive_vars_merged_vaccine(
  dataset=face,
  dataset_ex=ex,
  dataset_supp = NULL,
  dataset_suppex = NULL,
  ex_vars = exprs(EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC,VISIT,VISITNUM))


# Basic Filter and Pre-processing for ADFACE

adface <- adface %>%
  filter(FACAT == 'REACTOGENICITY' & grepl('ADMIN|SYS',FASCAT)) %>%
  convert_blanks_to_na() %>%
  mutate(
    FAOBJ = str_to_sentence(FAOBJ)
  )

# Deriving Fever records

adface <- derive_param_fever_occur(dataset = adface,
                                   source_data = vs,
                                   faobj = "Fever")


# Creating ADT, ATM, ADTM

adface <- adface %>%
  derive_vars_dt(new_vars_prefix = "A",
                 dtc = FADTC) %>%
  derive_vars_dtm(new_vars_prefix = "A",
                  dtc = FADTC)

# Creating the direct mapping variables (AVAL, AVALC)

adface <- adface %>%
  mutate(
    AVAL = FAORRES,
    AVAL = as.numeric(AVAL),
    AVALC = as.character(FAORRES),
    ATPTREF = FATPTREF,
    ATPT = FATPT,
    ATPTN = FATPTNUM
  )

# Creating severity records from Diameter for Redness and Swelling

adface <- derive_param_diam_to_sev(
  dataset = adface,
  filter_diam = c("DIAMETER","LDIAM"),
  filter_faobj = c("Redness", "Swelling","Erythema"),
  testcd_sev = "SEV",
  test_sev = "Severity/Intensity",
  none = c(0, 2),
  mild = c(2, 5),
  mod = c(5, 10),
  sev = c(10)
)

# Deriving Maximum Severity

adface <- derive_param_maxsev(
  dataset = adface,
  exclude_events = NULL,
  filter_sev = "SEV",
  test_maxsev = "Maximum Severity",
  testcd_maxsev = "MAXSEV",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Deriving Maximum Diameter

adface <- derive_param_maxdiam(
  dataset = adface,
  filter = FAOBJ %in% c("REDNESS","SWELLING") & FATESTCD == "DIAMETER",
  by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
  test_maxdiam = "Maximum Diameter",
  testcd_maxdiam = "MAXDIAM"
)

# Deriving Maximum Temperature

adface <- derive_param_maxtemp(dataset = adface,
                               filter_faobj = "Fever",
                               test_maxtemp = "Maximum Temperature",
                               testcd_maxtemp = "MAXTEMP",
                               by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
)

# Assigning PARAM, PARAMN, PARAMCD, PARCAT1 and PARCAT2 by Lookup table

lookup_dataset <-
  read_xlsx("C:/Users/RUBALA/OneDrive - Pfizer/Desktop/Admiral_vaccine/A/MRdata/adfacevdf_lookup.xlsx") %>%
  mutate(FAOBJ=str_to_sentence(FAOBJ),
         FATESTCD=toupper(FATESTCD))

adface <- derive_vars_params(dataset = adface,
                             lookup_dataset = lookup_dataset)

# Maximum flag ANL01FL and ANL02FL

adface <- derive_vars_max_flag(dataset = adface,
                               flag1 = "ANL01FL",
                               flag2 = "ANL02FL")

# Creating flag variables for an occured events

adface <- derive_vars_event_flag(dataset = adface,
                                 by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
                                 aval_cutoff = 2.5,
                                 new_var1 = EVENTL,
                                 new_var2 = EVENTDL
)

# Merging ADFACE with ADSL

adface <- derive_vars_merged(
  dataset = adface,
  dataset_add = adsl,
  by_vars = exprs(STUDYID,USUBJID)
)


keep_vars <- c(
  "SUBJID", "SITEID", "AGE", "AGEU", "SEX", "SEXN", "RACE", "RACEN", "ARACE",
  "ARACEN", "SAFFL", "COMPLFL", "ARM", "ARMCD", "ACTARM", "ACTARMCD", "ARMNRS",
  "ACTARMUD", "DARM", "DARMCD", "DACTARM", "DACTARCD", "TRTSDT", "TRTSDTM",
  "TRTSTM", "TRTEDT", "TRTEDTM", "TRTETM",
  "STUDYID", "USUBJID", "SUBJID", "SITEID", "ARM", "ARMCD", "ACTARMCD", "ACTARM",
  "ARMNRS", "ACTARMUD", "DARM", "DARMCD", "DACTARM", "DACTARCD", "TRTSDT", "TRTEDT",
  "AGE", "AGEU", "SEX", "SEXN", "RACE", "RACEN", "SAFFL", "SRCDOM", "SRCSEQ", "FATEST",
  "FAGRPID", "FALNKID", "FALNKGRP", "FATESTCD", "PARAMCD", "PARAM", "PARAMN", "FAOBJ",
  "FALLT", "FAPTCD", "FADECOD", "FABODSYS", "FABDSYCD", "PARCAT1", "PARCAT2", "AVALC",
  "AVAL", "AVALCAT1", "AVALCA1N", "FASTAT", "FAREASND", "FAEVAL", "AVISITN", "AVISIT",
  "EPOCH", "ADT", "ADTM", "FAEVINTX", "DTYPE", "FASTINT", "FAENINT", "ADY", "ATPT", "ATPTN",
  "ATPTREF", "ATPTREFN", "EXDOSE", "EXTRT", "EXDOSU", "EXSTDTC", "EXENDTC", "CLTYP", "VSSTRESN",
  "VSSTRESU", "FTEMCAT", "FTEMCATN", "KNOWVFL", "EVENTFL", "EVENTDFL", "KNOWVDFL", "UNPLSRC",
  "BIPHASFL", "EVENTOCC", "TRTA", "TRTAN", "TRTP", "TRTPN", "APERIOD", "APERIODC",
  "APERSDT", "APERSTM", "APERSDTM", "APEREDT", "", "APERETM","APEREDTM","APERDY",
  "FAORRES"
)
adface_final <- adface %>% select(any_of(keep_vars),starts_with("TRT0"),starts_with("VAX"),
                                  starts_with("EVE"),starts_with("ANL"))


# Save output
saveRDS(adface_final, file = file.path(dir, "adface.rds"), compress = "bzip2")
