# Creating ADCE

## Introduction

This article describes creating an `ADCE` ADaM for the analysis of
Vaccine Reactogenicity Data collected in SDTM `CE` domain. The current
presented example is tested using `CE` SDTM domains and `ADSL` ADaM
domain. However, other domains could be used if needed (eg temperature
data collected in `VS`).

**Note:** *All examples assume CDISC SDTM and/or ADaM format as input
unless otherwise specified.*

## Programming Flow

- [Read in Data](#readdata)
- [Pre-processing of Input Dataset](#cefilter)
- [Create Reference Dataset for Periods](#adperiods)
- [Derivation of Analysis Dates](#adates)
- [Join with the Periods Reference Dataset and Derive Relative Day in
  Period](#mergeperiods)
- [Creation of Analysis Version for GRADING Variable (Either `TOXGR` or
  `SEV`)](#agrade)
- [Creation of Analysis Sequence Number](#aseq)
- [Final Step : Get All the Remaining Variables from
  `ADSL`](#jadsl_list)

### Read in Data

Assumption: The `CE` domain has already been merged with the `SUPPCE`
dataset. If this is not yet the case, join `SUPPCE` onto parent `CE`
domain using `metatools::combine_supp(CE, SUPPCE)`.

``` r
library(admiraldev)
library(admiral)
library(dplyr)
library(lubridate)
library(admiralvaccine)
library(pharmaversesdtm)

data("ce_vaccine")
data("admiralvaccine_adsl")

adsl <- admiralvaccine_adsl
ce <- ce_vaccine

ce <- convert_blanks_to_na(ce)
adsl <- convert_blanks_to_na(adsl)
```

### Pre-processing of Input Dataset

This step involves company-specific pre-processing of required input
dataset for further analysis. In this step, we will filter records that
has only reactogenicity events.

``` r
adce <- ce %>%
  filter(CECAT == "REACTOGENICITY")
```

### Create Reference Dataset for Periods

Create period dataset - for joining period information onto `CE`
records. Need to remove datetime variables as otherwise causes duplicate
issues.

``` r
adsl2 <- adsl %>%
  select(-c(starts_with("AP") & ends_with("DTM")))

adperiods <- create_period_dataset(
  adsl2,
  new_vars = exprs(APERSDT = APxxSDT, APEREDT = APxxEDT)
)
```

### Derivation of Analysis Dates

At this step, it may be useful to join `ADSL` to your `CE` domain. Only
the `ADSL` variables used for derivations are selected at this step. The
rest of the relevant `ADSL` variables would be added later.

``` r
adsl_vars <- exprs(TRTSDT, TRTEDT)

adce <- adce %>%
  # join ADSL to CE
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by = get_admiral_option("subject_keys")
  ) %>%
  derive_vars_dt(
    dtc = CESTDTC,
    new_vars_prefix = "AST",
    highest_imputation = "n"
  ) %>%
  derive_vars_dt(
    dtc = CEENDTC,
    new_vars_prefix = "AEN",
    highest_imputation = "n"
  ) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(ASTDT, AENDT)
  )
```

### Join with the Periods Reference Dataset and Derive Relative Day in Period

Also add analysis version of `CEREL`(`AREL`).

``` r
adce <-
  derive_vars_joined(
    adce,
    dataset_add = adperiods,
    by_vars = get_admiral_option("subject_keys"),
    filter_join = ASTDT >= APERSDT & ASTDT <= APEREDT,
    join_type = "all"
  ) %>%
  mutate(
    APERSTDY = as.integer(ASTDT - APERSDT) + 1,
    AREL = CEREL
  )
```

### Creation of Analysis Version for GRADING Variable (Either `TOXGR` or `SEV`)

Depending on which variable is collected for the Grading (`TOXGR` or
`SEV`) in `CE` domain, derive the associated analysis version. In
current example, `SEV` is collected, so the code is using this as an
example. In addition, derivation of Extreme Flags: in current example:
flag the first occurrence of the most severe grade within a Period
(`AOCC01FL`).

``` r
adce <- adce %>%
  mutate(
    ASEV = CESEV,
    ASEVN = as.integer(factor(ASEV,
      levels = c("MILD", "MODERATE", "SEVERE", "DEATH THREATENING")
    ))
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(USUBJID, APERIOD),
      order = exprs(desc(ASEVN), ASTDY, CEDECOD),
      new_var = AOCC01FL,
      mode = "first"
    ),
    filter = !is.na(APERIOD) & !is.na(ASEV)
  )
```

### Creation of Analysis Sequence Number

``` r
adce <- adce %>%
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(CEDECOD, CELAT, CETPTREF, APERIOD),
    check_type = "error"
  ) %>%
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
```

### Final Step : Get All the Remaining Variables from `ADSL`

Get list of `ADSL` vars as per trial specific which needs to be adjusted
when using the template

``` r
adsl_list <- adsl %>%
  select(STUDYID, USUBJID, TRT01A, TRT01P, AGE, AGEU, SEX, RACE, COUNTRY, ETHNIC, SITEID, SUBJID)

adce <- adce %>%
  derive_vars_merged(
    dataset_add = adsl_list,
    by_vars = get_admiral_option("subject_keys")
  )
```

## Example Script

| ADaM | Sample Code                                                                                   |
|------|-----------------------------------------------------------------------------------------------|
| ADCE | [ad_adce.R](https://github.com/pharmaverse/admiralvaccine/blob/main/inst/templates/ad_adce.R) |
