# STEP 8 Derivation of Change from baseline and Ratio to baseline ----
is8 <- is7 %>%
       derive_var_chg %>%
       derive_var_analysis_ratio(numer_var = AVAL, denom_var = BASE)

# STEP 9 Derivation of CRITyFL and CRITyFN ----

is9 <- is8 %>%
  derive_vars_crit(new_var = "CRIT1",
     label_var = "Titer >= ISLLOQ",
     condition = !is.na(AVAL) & !is.na(ISLLOQ),
     criterion = AVAL >= ISLLOQ)

# STEP 10  Merge with ADSL ----

# Get list of ADSL variables not to be added to ADIS
adsl_vars <- exprs(RFSTDTC)

is10 <- is9 %>%
  derive_vars_merged(
    dataset_add = select(vx_adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  )

# STEP 11 Derivation of TRTP/A treatment variables ----

is11 <- is10 %>%
    mutate(TRTP=TRT01P,TRTA=TRT01A)

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
