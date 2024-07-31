# Name: ADCE
#
# Label: Clinical Event Analysis Dataset
#
# Input: CE, ADSL, VS
library(admiral)
library(dplyr)
library(lubridate)
library(admiralvaccine)
library(pharmaversesdtm)


# Load source datasets

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("ce_vaccine")
data("admiralvaccine_adsl")


adsl <- admiralvaccine_adsl
ce <- ce_vaccine


# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint

ce <- convert_blanks_to_na(ce)
adsl <- convert_blanks_to_na(adsl)

# Derivations
# Get CE records
adce01 <- ce %>%
  filter(CECAT == "REACTOGENICITY")

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT)

# Create period dataset - for joining period information onto CE records
# Need to remove datetime variables as otherwise causes duplicate issues
adsl2 <- adsl %>%
  select(-c(starts_with("AP") & ends_with("DTM")))

adperiods <- create_period_dataset(
  adsl2,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT)
)

# Derive analysis dates/days
adce02 <- adce01 %>%
  # join ADSL to CE
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = get_admiral_option("subject_keys")
  ) %>%
  ## Derive analysis start time
  ## Proposed imputations depending on situation: no needed -> highest imputation = “n”
  ## some missing dates: highest imputation = “D”
  derive_vars_dt(
    dtc = CESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "n"
  ) %>%
  ## Derive analysis end time
  derive_vars_dt(
    dtc = CEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "n"
  ) %>%
  ## Derive analysis start relative day and  analysis end relative day
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(ASTDT, AENDT)
  )



adce03 <-
  derive_vars_joined(
    adce02,
    dataset_add = adperiods,
    by_vars = get_admiral_option("subject_keys"),
    filter_join = ASTDT >= APERSDT & ASTDT <= APEREDT,
    join_type = "all"
  ) %>%
  mutate(
    APERSTDY = as.integer(ASTDT - APERSDT) + 1,
    AREL = CEREL
  )



adce04 <- adce03 %>%
  ## depending on collection of TOXGR or SEV in CE domain
  ## Analysis variant of ASEV and ASEVN
  mutate(
    ASEV = CESEV,
    ASEVN = as.integer(factor(ASEV,
      levels = c("MILD", "MODERATE", "SEVERE", "DEATH THREATENING")
    ))
  ) %>%
  ## Derive occurrence flags: first occurrence of most severe solicited AE
  ## - Company specific
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, APERIOD),
      order = exprs(desc(ASEVN), ASTDY, CEDECOD, CESEQ),
      new_var = AOCC01FL,
      mode = "first"
    ),
    filter = !is.na(APERIOD) & !is.na(ASEV)
  )

adce05 <- adce04 %>%
  ## Derive ASEQ
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(CEDECOD, CELAT, CETPTREF, APERIOD),
    check_type = "error"
  ) %>%
  ## Derive analysis duration (value and unit)
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

# Get list of ADSL vars, list is trial specific and needs to be adjusted when using the template
adsl_list <- adsl %>%
  select(STUDYID, USUBJID, TRT01A, TRT01P, AGE, AGEU, SEX, RACE, COUNTRY, ETHNIC, SITEID, SUBJID)


# Join ADSL_list with CE
adce <- adce05 %>%
  derive_vars_merged(
    dataset_add = adsl_list,
    by_vars = get_admiral_option("subject_keys")
  )


admiralvaccine_adce <- adce

# Save output ----

dir <- tools::R_user_dir("admiralvaccine_templates_data", which = "cache")
# Change to whichever directory you want to save the dataset in
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(admiralvaccine_adce, file = file.path(dir, "adce.rda"), compress = "bzip2")
