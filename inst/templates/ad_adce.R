# Name: ADCE
#
# Label: Clinical Event Analysis Dataset
#
# Input: ce, adsl, vs
library(admiral)
library(dplyr)
library(lubridate)
library(haven)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

adsl <- read_sas("./data/adsl.sas7bdat")
ce <- read_sas("./data/ce.sas7bdat")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

ce <- convert_blanks_to_na(ce)

# Derivations ----
# Get CE records
ce01 <- ce %>%
    filter(CECAT=="REACTOGENICITY") #& CEOCCUR=="Y" & !is.na(CETOXGR) & CEGRPID != "IMMEDIATE REACTION")

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT,TRTEDT)

# Create period dataset - for joining period information onto ce records
# Need to remove datetime variables as otherwise causes duplicate issues - to be looked into?
adsl2<- adsl %>%
  select(-c(AP01SDTM, AP01EDTM, AP02SDTM, AP02EDTM, AP06SDTM, AP06EDTM))

adperiods <- create_period_dataset(
  adsl2,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT)
)

# Derive analysis dates/days
ce02 <- ce01 %>%
  # join adsl to ce
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = exprs(STUDYID, USUBJID)
  ) %>%
  ## Derive analysis start time ----
derive_vars_dtm(
  dtc = CESTDTC,
  new_vars_prefix = "AST",
  highest_imputation = "M",
  min_dates = exprs(TRTSDT)
) %>%
  ## Derive analysis end time ----
derive_vars_dtm(
  dtc = CEENDTC,
  new_vars_prefix = "AEN",
  highest_imputation = "M"
) %>%
  ## Derive analysis end/start date ----
derive_vars_dtm_to_dt(exprs(ASTDTM, AENDTM)) %>%
  ## Derive analysis start relative day and  analysis end relative day ----
derive_vars_dy(
  reference_date = TRTSDT,
  source_vars = exprs(ASTDT, AENDT)
)

ce03 <-
  derive_vars_joined(
  ce02,
  dataset_add = adperiods,
  by_vars = exprs(USUBJID),
  filter_join = ASTDT >= APERSDT & ASTDT <= APEREDT
) %>%
  mutate(APERSTDY=as.integer(ASTDT-APERSDT)+1,
         ADECOD = CEDECOD,
         ATOXGR = CETOXGR,
         AREL = CEREL#,
         #ASEV = AESEV
  ) %>%
# create numeric value ASEVN for severity and ATOXGRN for toxicity
  mutate(
    ATOXGRN = as.integer(factor(ATOXGR, levels = c("0", "1", "2", "3", "4"))),
    #ASEVN = as.integer(factor(ASEV, levels = c("MILD", "MODERATE", "SEVERE", "DEATH THREATENING")))
  ) %>%

## Derive occurrence flags: first occurrence of most severe solicited AE - Janssen specific
restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = exprs(USUBJID,APERIOD),
    order = exprs(desc(ATOXGRN), CESTDY, CEDECOD),
    new_var = AOCC01FL,
    mode = "first"
  ),
  filter = !is.na(APERIOD) & !is.na(ATOXGR)
)

ce04 <-
  ## Derive ASEQ ----
derive_var_obs_number(
  ce03,
  by_vars = exprs(USUBJID, APERIOD),
  order = exprs(ADECOD)
) %>%
  ## Derive analysis duration (value and unit) ----
derive_vars_duration(
  new_var = ADURN,
  new_var_unit = ADURU,
  start_date = ASTDT,
  end_date = AENDT,
  in_unit = "days",
  out_unit = "days",
  add_one = TRUE,
  trunc_out = FALSE
)



# Join all ADSL with CE
adce <- ce04 %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  mutate(APERSTDY=as.integer(ASTDT-APERSDT)+1)


# Save output ----

dir <- tempdir() # Change to whichever directory you want to save the dataset in
saveRDS(adce, file = file.path(dir, "adce.rds"), compress = "bzip2")
