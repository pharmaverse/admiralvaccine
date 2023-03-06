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


# Load source datasets ----
data("is")
data("suppis")
data("admiral_adsl")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint


is <- convert_blanks_to_na(is)
suppis <- convert_blanks_to_na(suppis)
adsl <- convert_blanks_to_na(admiral_adsl)


# Derivations ----

# STEP 1 - combine IS with SUPPIS.
# Please, upload MOCK data
is_suppis <- combine_supp(is, suppis)


# STEP 2 - Visits and timing vriables derivation.
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
# If this step is not applicable for your purposes then skip to the next code section:
# (substitute is_log with is2_dy in line 116)

is_log <- is2_ady %>%
  mutate(DERIVED = "Y") %>%
  bind_rows(is2_ady) %>%
  arrange(STUDYID, USUBJID, ISSEQ, !is.na(DERIVED))


is3 <- is_log %>%
  mutate(
    # PARAMCD: for log values, concatenation of L and ISTESTCD.
    PARAMCD = if_else(is.na(DERIVED), ISTESTCD, paste0(ISTESTCD, "L"))
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
  "R0003MAL", "LOG10 (R0003MA Antibody)", 14
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
