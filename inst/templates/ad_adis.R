# Name: ADIS
#
# Label: Immunogenicity Analysis
#
# Input: is, suppis, adsl
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(metatools)
library(dplyr)
library(lubridate)
library(rlang)
library(admiralvaccine)



# Load source datasets ----
data("vx_is")
data("vx_suppis")
data("vx_adsl")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint


is <- convert_blanks_to_na(vx_is)
suppis <- convert_blanks_to_na(vx_suppis)
adsl <- convert_blanks_to_na(vx_adsl)


# Derivations ----

# STEP 1 - combine IS with SUPPIS.
# Please, upload MOCK data
is_suppis <- combine_supp(is, suppis)


# STEP 2 - Visits and timing variables derivation.
adis <- is_suppis %>%
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
      VISITNUM == 20 ~ "Visit 2 (Day 31)",
      VISITNUM == 30 ~ "Visit 3 (Day 61)",
      VISITNUM == 40 ~ "Visit 4 (Day 121)",
      is.na(VISITNUM) ~ NA_character_
    ),
    ATPTREF = case_when(
      VISITNUM %in% c(10, 20) ~ "FIRST TREATMENT",
      VISITNUM %in% c(30, 40) ~ "SECOND TREATMENT",
      is.na(VISITNUM) ~ NA_character_
    )
  )


# STEP 3: ADT and ADY derivation

# ADT derivation and Merge with ADSL to get RFSTDTC info in order to derive ADY
# Add also PPROTFL from ADSL (to avoid additional merges) in order to derive
# PPSRFL at step 11.
adis <- derive_vars_dt(
  dataset = adis,
  new_vars_prefix = "A",
  dtc = ISDTC,
  highest_imputation = "M",
  date_imputation = "mid",
  flag_imputation = "none"
) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(RFSTDTC, PPROTFL),
    by_vars = exprs(STUDYID, USUBJID),
  ) %>%
  mutate(
    ADT = as.Date(ADT),
    RFSTDTC = as.Date(RFSTDTC)
  ) %>%
  # ADY derivation
  derive_vars_dy(
    reference_date = RFSTDTC,
    source_vars = exprs(ADT)
  )

# STEP 4: PARAMCD, PARAM and PARAMN derivation

# Create record duplication in order to plot both original and LOG10 parameter values.
# Add also records related to 4fold.
# Please, keep or modify PARAM values according to your purposes.

is_log <- adis %>%
  mutate(DERIVED = "LOG10")

is_4fold <- adis %>%
  mutate(DERIVED = "4FOLD")

is_log_4fold <- adis %>%
  mutate(DERIVED = "LOG10 4FOLD")

adis <- bind_rows(adis, is_log, is_4fold, is_log_4fold) %>%
  arrange(STUDYID, USUBJID, VISITNUM, ISSEQ, !is.na(DERIVED)) %>%
  mutate(DERIVED = if_else(is.na(DERIVED), "ORIG", DERIVED))


adis <- adis %>%
  mutate(
    # PARAMCD: for log values, concatenation of L and ISTESTCD.
    PARAMCD = case_when(
      DERIVED == "ORIG" ~ ISTESTCD,
      DERIVED == "LOG10" ~ paste0(ISTESTCD, "L"),
      DERIVED == "4FOLD" ~ paste0(ISTESTCD, "F"),
      # As per CDISC rule, PARAMCD should be 8 charcaters long. Please, adapt if needed
      DERIVED == "LOG10 4FOLD" ~ paste0(substr(ISTESTCD, 1, 6), "LF")
    )
  )


# Update param_lookup dataset with your PARAM values.
param_lookup <- tribble(
  ~PARAMCD, ~PARAM, ~PARAMN,
  "J0033VN", "J0033VN Antibody", 1,
  "I0019NT", "I0019NT Antibody", 2,
  "M0019LN", "M0019LN Antibody", 3,
  "R0003MA", "R0003MA Antibody", 4,
  "J0033VNL", "LOG10 (J0033VN Antibody)", 11,
  "I0019NTL", "LOG10 (I0019NT Antibody)", 12,
  "M0019LNL", "LOG10 (M0019LN Antibody)", 13,
  "R0003MAL", "LOG10 (R0003MA Antibody)", 14,
  "J0033VNF", "4FOLD (J0033VN Antibody)", 21,
  "I0019NTF", "4FOLD (I0019NT Antibody)", 22,
  "M0019LNF", "4FOLD (M0019LN Antibody)", 23,
  "R0003MAF", "4FOLD (R0003MA Antibody)", 24,
  "J0033VLF", "LOG10 4FOLD (J0033VN Antibody)", 31,
  "I0019NLF", "LOG10 4FOLD (I0019NT Antibody)", 32,
  "M0019LLF", "LOG10 4FOLD (M0019LN Antibody)", 33,
  "R0003MLF", "LOG10 4FOLD (R0003MA Antibody)", 34
)

adis <- derive_vars_merged_lookup(
  dataset = adis,
  dataset_add = param_lookup,
  new_vars = exprs(PARAM, PARAMN),
  by_vars = exprs(PARAMCD)
)

# STEP 5: PARCAT1 and CUTOFF0x derivations.
adis <- adis %>%
  mutate(
    PARCAT1 = ISCAT,
    # Please, define your additional cutoff values. Delete if not needed.
    CUTOFF02 = 4,
    CUTOFF03 = 8
  )


# STEP 6: AVAL, AVALU, DTYPE and SERCAT1/N derivation
# AVAL derivation
adis_or <- adis %>%
  filter(DERIVED == "ORIG") %>%
  derive_var_aval_adis(
    lower_rule = ISLLOQ / 2,
    middle_rule = ISSTRESN,
    upper_rule = ISULOQ,
    round = 2
  )

adis_log_or <- adis %>%
  filter(DERIVED == "LOG10") %>%
  derive_var_aval_adis(
    lower_rule = log10(ISLLOQ / 2),
    middle_rule = log10(ISSTRESN),
    upper_rule = log10(ISULOQ),
    round = 2
  )

adis_4fold <- adis %>%
  filter(DERIVED == "4FOLD") %>%
  derive_var_aval_adis(
    lower_rule = ISLLOQ,
    middle_rule = ISSTRESN,
    upper_rule = ISULOQ,
    round = 2
  )

adis_log_4fold <- adis %>%
  filter(DERIVED == "LOG10 4FOLD") %>%
  derive_var_aval_adis(
    lower_rule = log10(ISLLOQ),
    middle_rule = log10(ISSTRESN),
    upper_rule = log10(ISULOQ),
    round = 2
  )

adis <- bind_rows(adis_or, adis_log_or, adis_4fold, adis_log_4fold) %>%
  mutate(
    # AVALU derivation (please delete if not needed for your study)
    AVALU = ISSTRESU,

    # SERCAT1 derivation
    SERCAT1 = case_when(
      ISBLFL == "Y" & !is.na(AVAL) & !is.na(ISLLOQ) & AVAL < ISLLOQ ~ "S-",
      ISBLFL == "Y" & !is.na(AVAL) & !is.na(ISLLOQ) & AVAL >= ISLLOQ ~ "S+",
      ISBLFL == "Y" & (is.na(AVAL) | is.na(ISLLOQ)) ~ "UNKNOWN"
    )
  )


# Update param_lookup2 dataset with your SERCAT1N values.
param_lookup2 <- tribble(
  ~SERCAT1, ~SERCAT1N,
  "S-", 1,
  "S+", 2,
  "UNKNOWN", 3,
  NA_character_, NA_real_
)

adis <- derive_vars_merged_lookup(
  dataset = adis,
  dataset_add = param_lookup2,
  new_vars = exprs(SERCAT1N),
  by_vars = exprs(SERCAT1)
)


# DTYPE derivation.
# Please update code when <,<=,>,>= are present in your lab results (in ISSTRESC)

if (any(names(adis) == "ISULOQ") == T) {
  adis <- adis %>%
    mutate(DTYPE = case_when(
      DERIVED %in% c("ORIG", "LOG10") & !is.na(ISLLOQ) &
        ((ISSTRESN < ISLLOQ) | grepl("<", ISORRES)) ~ "HALFLLOQ",
      DERIVED %in% c("ORIG", "LOG10") & !is.na(ISULOQ) &
        ((ISSTRESN > ISULOQ) | grepl(">", ISORRES)) ~ "ULOQ",
      TRUE ~ NA_character_
    ))
}

if (any(names(adis) == "ISULOQ") == F) {
  adis <- adis %>%
    mutate(DTYPE = case_when(
      DERIVED %in% c("ORIG", "LOG10") & !is.na(ISLLOQ) &
        ((ISSTRESN < ISLLOQ) | grepl("<", ISORRES)) ~ "HALFLLOQ",
      TRUE ~ NA_character_
    ))
}


# STEP 7: ABLFL and BASE variables derivation
# ABLFL derivation
adis <- restrict_derivation(
  adis,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = exprs(STUDYID, USUBJID, PARAMN),
    order = exprs(STUDYID, USUBJID, VISITNUM, PARAMN),
    new_var = ABLFL,
    mode = "first"
  ),
  filter = VISITNUM == 10
) %>%
  # BASE derivation
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMN),
    source_var = AVAL,
    new_var = BASE,
    filter = ABLFL == "Y"
  ) %>%
  # BASETYPE derivation
  derive_var_basetype(
    basetypes = exprs("VISIT 1" = AVISITN %in% c(10, 30))
  ) %>%
  arrange(STUDYID, USUBJID, DERIVED)


# BASECAT derivation
adis <- adis %>%
  mutate(
    BASECAT1 = case_when(
      !grepl("L", PARAMCD) & BASE < 10 ~ "Titer value < 1:10",
      !grepl("L", PARAMCD) & BASE >= 10 ~ "Titer value >= 1:10",
      grepl("L", PARAMCD) & BASE < 10 ~ "Titer value < 1:10",
      grepl("L", PARAMCD) & BASE >= 10 ~ "Titer value >= 1:10"
    )
  )


# STEP 8 Derivation of Change from baseline and Ratio to baseline ----
adis <- restrict_derivation(adis,
  derivation = derive_var_chg,
  filter = AVISITN > 10
) %>%
  restrict_derivation(
    derivation = derive_var_analysis_ratio,
    args = params(
      numer_var = AVAL,
      denom_var = BASE
    ),
    filter = AVISITN > 10
  ) %>%
  arrange(STUDYID, USUBJID, DERIVED, ISSEQ)


# STEP 9 Derivation of CRITyFL and CRITyFN ----
adis <- derive_vars_crit(
  dataset = adis,
  new_var = "CRIT1",
  label_var = "Titer >= ISLLOQ",
  condition = !is.na(AVAL) & !is.na(ISLLOQ),
  criterion = AVAL >= ISLLOQ
)


# STEP 10 Derivation of TRTP/A treatment variables ----
period_ref <- create_period_dataset(
  dataset = adsl,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT, TRTA = TRTxxA)
)

adis <- derive_vars_joined(
  adis,
  dataset_add = period_ref,
  by_vars = exprs(STUDYID, USUBJID),
  filter_join = ADT >= APERSDT & ADT <= APEREDT
)


# STEP 11 Derivation of PPSRFL ----
is12a <- adis %>%
  filter(VISITNUM == 10) %>%
  derive_var_merged_exist_flag(
    dataset_add = vx_adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = PPSRFL,
    condition = PPROTFL == "Y",
    true_value = "Y"
  )

is12b <- adis %>%
  filter(VISITNUM == 30) %>%
  derive_var_merged_exist_flag(
    dataset_add = vx_adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = PPSRFL,
    condition = PPROTFL == "Y",
    true_value = "Y"
  )

adis <- bind_rows(is12a, is12b)


# STEP 12  Merge with ADSL ----

# Get list of ADSL variables not to be added to ADIS
vx_adsl_vars <- exprs(RFSTDTC, PPROTFL)

adis <- derive_vars_merged(
  dataset = adis,
  dataset_add = select(vx_adsl, !!!negate_vars(vx_adsl_vars)),
  by_vars = exprs(STUDYID, USUBJID)
)


# Save output ----
dir <- tempdir() # Change to whichever directory you want to save the dataset in
saveRDS(adis, file = file.path(dir, "adis.rda"), compress = "bzip2")
