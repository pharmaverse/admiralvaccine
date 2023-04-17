# Name: ADIS
#
# Label: Immunogenicity Analysis
#
# Input: is, suppis, adsl
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
install.packages("metatools", repos = "https://cloud.r-project.org")
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
is1 <- is_suppis %>%
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
      VISITNUM == 20 ~ "Visit 2 (Day #)",
      VISITNUM == 30 ~ "Visit 3 (Day #)",
      VISITNUM == 40 ~ "Visit 4 (Day #)",
      is.na(VISITNUM) ~ NA_character_
    ),
    ATPTREF = case_when(
      VISITNUM %in% c(10, 20) ~ "FIRST TREATMENT",
      VISITNUM %in% c(30, 40) ~ "SECOND TREATMENT",
      is.na(VISITNUM) ~ NA_character_
    )
  )


# STEP 3: ADT and ADY derivation

# Tried to test this function with missing days and/or months and missing dates:
# To put highest_imputation = "M" and date_imputation = "mid" to be in line with GSK rules.
# flag_imputation = "none" to suppress ADTF variable.

# ADT derivation
is2_adt <- derive_vars_dt(
  dataset = is1,
  new_vars_prefix = "A",
  dtc = ISDTC,
  highest_imputation = "M",
  date_imputation = "mid",
  flag_imputation = "none"
)


# Merge with ADSL to get RFSTDTC info in order to derive ADY
is2_rf <- derive_var_merged_character(
  dataset = is2_adt,
  dataset_add = adsl,
  by_vars = exprs(STUDYID, USUBJID),
  new_var = RFSTDTC,
  source_var = RFSTDTC
) %>%
  mutate(
    ADT = as.Date(ADT),
    RFSTDTC = as.Date(RFSTDTC)
  )


# ADY derivation
is2_ady <- derive_vars_dy(
  dataset = is2_rf,
  reference_date = RFSTDTC,
  source_vars = exprs(ADT)
)



# STEP 4: PARAMCD, PARAM and PARAMN derivation

# Create record duplication in order to plot both original and LOG10 parameter values.
# Add also records related to 4fold.
# Please, keep or modify PARAM values according to your purposes.

is_log <- is2_ady %>%
  mutate(DERIVED = "LOG10")

is_4fold <- is2_ady %>%
  mutate(DERIVED = "4FOLD")

is_log_4fold <- is2_ady %>%
  mutate(DERIVED = "LOG10 4FOLD")

is_derived <- bind_rows(is2_ady, is_log, is_4fold, is_log_4fold) %>%
  arrange(STUDYID, USUBJID, VISITNUM, ISSEQ, !is.na(DERIVED)) %>%
  mutate(DERIVED = if_else(is.na(DERIVED), "ORIG", DERIVED))


is3 <- is_derived %>%
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

is3_1 <- derive_vars_merged_lookup(
  dataset = is3,
  dataset_add = param_lookup,
  new_vars = exprs(PARAM),
  by_vars = exprs(PARAMCD)
)

is4 <- derive_vars_merged_lookup(
  dataset = is3_1,
  dataset_add = param_lookup,
  new_vars = exprs(PARAMN),
  by_vars = exprs(PARAM)
)

# STEP 5: PARCAT1 and CUTOFF0x derivations.
is5 <- is4 %>%
  mutate(
    PARCAT1 = ISCAT,
    # Please, define your additional cutoff values. Delete if not needed.
    CUTOFF02 = 4,
    CUTOFF03 = 8
  )


# STEP 6: AVAL, AVALU, DTYPE and SERCAT1/N derivation
# AVAL derivation
is5_aval <- is5 %>%
  mutate(
    AVAL = case_when(
      # ISORRES values without > or <
      DERIVED == "ORIG" & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ / 2,
      DERIVED == "ORIG" & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ULLOQ
      ~ ISSTRESN,
      DERIVED == "ORIG" & !is.na(ISSTRESN) & ISSTRESN >= ULLOQ ~ ULLOQ,
      DERIVED == "LOG10" & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ log10(ISLLOQ / 2),
      DERIVED == "LOG10" & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ULLOQ
      ~ log10(ISSTRESN),
      DERIVED == "LOG10" & !is.na(ISSTRESN) & ISSTRESN >= ULLOQ ~ log10(ULLOQ),
      DERIVED == "4FOLD" & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ ISLLOQ,
      DERIVED == "4FOLD" & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ULLOQ
      ~ ISSTRESN,
      DERIVED == "4FOLD" & !is.na(ISSTRESN) & ISSTRESN >= ULLOQ ~ ULLOQ,
      DERIVED == "LOG10 4FOLD" & !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ log10(ISLLOQ),
      DERIVED == "LOG10 4FOLD" & !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ &
        ISSTRESN < ULLOQ ~ log10(ISSTRESN),
      DERIVED == "LOG10 4FOLD" & !is.na(ISSTRESN) & ISSTRESN >= ULLOQ ~ log10(ULLOQ),

      # ISORRES values with > or <
      DERIVED == "ORIG" & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ / 2,
      DERIVED == "ORIG" & grepl(">", ISORRES) & !is.na(ISORRES) ~ ULLOQ,
      DERIVED == "LOG10" & grepl("<", ISORRES) & !is.na(ISORRES) ~ log10(ISLLOQ / 2),
      DERIVED == "LOG10" & grepl(">", ISORRES) & !is.na(ISORRES) ~ log10(ULLOQ),
      DERIVED == "4FOLD" & grepl("<", ISORRES) & !is.na(ISORRES) ~ ISLLOQ,
      DERIVED == "4FOLD" & grepl(">", ISORRES) & !is.na(ISORRES) ~ ULLOQ,
      DERIVED == "LOG10 4FOLD" & grepl("<", ISORRES) & !is.na(ISORRES) ~ log10(ISLLOQ),
      DERIVED == "LOG10 4FOLD" & grepl(">", ISORRES) & !is.na(ISORRES) ~ log10(ULLOQ)
    ),

    # AVALU derivation (please delete if not needed for your study)
    AVALU = if_else(as.numeric(ISSTRESC) == ISSTRESN, ISORRESU, as.character(NA)),

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
  as.character(NA), as.numeric(NA)
)

is5_sercat1n <- derive_vars_merged_lookup(
  dataset = is5_aval,
  dataset_add = param_lookup2,
  new_vars = exprs(SERCAT1N),
  by_vars = exprs(SERCAT1)
)


# DTYPE derivation.
# Please update code when <,<=,>,>= are present in your lab results (in ISSTRESC)
# and/or ULOQ is present in your study
is6 <- is5_sercat1n %>%
  mutate(DTYPE = if_else(DERIVED %in% c("ORIG", "LOG10") & !is.na(ISLLOQ) & ISSTRESN < ISLLOQ,
    "HALFLLQ",
    as.character(NA)
  ))

# STEP 7: ABLFL and BASE variables derivation
# ABLFL derivation
is6_ablfl <- derive_var_relative_flag(
  dataset = is6,
  by_vars = exprs(STUDYID, USUBJID, PARAMN),
  order = exprs(STUDYID, USUBJID, VISITNUM, PARAMN),
  new_var = ABLFL,
  condition = VISITNUM == 10,
  mode = "first",
  selection = "before",
  inclusive = TRUE
)

# BASE derivation
is6_base <- derive_var_base(
  dataset = is6_ablfl,
  by_vars = exprs(STUDYID, USUBJID, PARAMN),
  source_var = AVAL,
  new_var = BASE,
  filter = ABLFL == "Y"
)

# BASETYPE derivation
is6_basetype <- derive_var_basetype(
  dataset = is6_base,
  basetypes = exprs("VISIT 1" = AVISITN %in% c(10, 30))
)


# BASECAT derivation
base_data <- is6_basetype %>%
  select(STUDYID, USUBJID, VISITNUM, PARAMCD, BASE) %>%
  distinct()

basecat1 <- function(base) {
  case_when(
    !grepl("L", base_data$PARAMCD) & base < 10 ~ "Titer value < 1:10",
    !grepl("L", base_data$PARAMCD) & base >= 10 ~ "Titer value >= 1:10",
    grepl("L", base_data$PARAMCD) & base < 10 ~ "Titer value < 1:10",
    grepl("L", base_data$PARAMCD) & base >= 10 ~ "Titer value >= 1:10"
  )
}

is7 <- derive_var_merged_cat(
  dataset = is6_basetype,
  dataset_add = base_data,
  by_vars = exprs(STUDYID, USUBJID, PARAMCD, VISITNUM),
  new_var = BASECAT1,
  source_var = BASE,
  cat_fun = basecat1
)

# STEP 8 Derivation of Change from baseline and Ratio to baseline ----
is8 <- is7 %>%
  derive_var_chg() %>%
  derive_var_analysis_ratio(numer_var = AVAL, denom_var = BASE)

# STEP 9 Derivation of CRITyFL and CRITyFN ----

is9 <- derive_vars_crit(
  dataset = is8,
  new_var = "CRIT1",
  label_var = "Titer >= ISLLOQ",
  condition = !is.na(AVAL) & !is.na(ISLLOQ),
  criterion = AVAL >= ISLLOQ
)

# STEP 10  Merge with ADSL ----

# Get list of ADSL variables not to be added to ADIS
vx_adsl_vars <- exprs(RFSTDTC)

is10 <- derive_vars_merged(
  dataset = is9,
  dataset_add = select(vx_adsl, !!!negate_vars(vx_adsl_vars)),
  by_vars = exprs(STUDYID, USUBJID)
)

# STEP 11 Derivation of TRTP/A treatment variables ----

is11 <- is10 %>%
  mutate(TRTP = TRT01P, TRTA = TRT01A)

# STEP 12 Derivation of PPSRFL ----

is12a <- is11 %>%
  filter(VISITNUM == 10) %>%
  derive_var_merged_exist_flag(
    dataset_add = vx_adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = PPSRFL,
    condition = PPROTFL == "Y",
    true_value = "Y"
  )

is12b <- is11 %>%
  filter(VISITNUM == 30) %>%
  derive_var_merged_exist_flag(
    dataset_add = vx_adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = PPSRFL,
    condition = PPROTFL == "Y",
    true_value = "Y"
  )

is12 <- bind_rows(is12a, is12b)
