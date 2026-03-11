# Creating ADFACE

## Introduction

This article describes about creating `ADFACE` ADaM dataset which is
part of Vaccine - Reactogenicity based on the Center for Biologics
Evaluation and Research (CBER) guidelines.

See the below links for more information:

[Center for Biologics Evaluation and Research (CBER)
Guidelines](https://www.fda.gov/media/112581/download)

[Therapeutic Area Data Standards User Guide for Vaccines
(TAUG-Vax)](https://www.cdisc.org/standards/therapeutic-areas/vaccines/vaccines-therapeutic-area-user-guide-v11/html)

Examples are currently tested using `ADSL` (ADaM) and `face`, `vs`, `ex`
(SDTM) inputs.

**Note**: *All examples assume CDISC SDTM and/or ADaM format as input
unless otherwise specified.*

## Programming Workflow

- [Read in Data](#readdata)
- [Pre-processing of Input Dataset](#input)
- [Merge `FACE` with `EX`](#merge)
- [Merge Required `ADSL` Variables Needed for Analysis](#adsl)
- [Derive Fever Records from `VS` Domain](#fever)
- [Derive/Impute Numeric Date/Time and Analysis Day (`ADT`, `ADTM`,
  `ADY`, `ADTF`, `ATMF`)](#datetime)
- [Derive Period Variables (e.g. `APxxSDT`, `APxxEDT`, …)](#periodvars)
- [Derive Direct Mapping Variables](#mapping)
- [Derive Severity Records for Administration Site Events](#sev)
- [Derive Maximum Records](#max)
- [Assign `PARAMCD`, `PARAM`, `PARAMN`, `PARCAT1`](#paramcd)
- [Derive Maximum Severity Flag](#maxflag)
- [Derive Event Occurrence Flag](#eventflag)
- [Post-processing of the Dataset](#post)
- [Add ADSL Variables](#adsl_vars)

### Read in Data

To start, all data frames needed for the creation of `ADFACE` should be
read into the environment. Some of the data frames needed are `VS`,`EX`
and `FACE`.

``` r
library(admiral)
library(admiralvaccine)
library(admiraldev)
library(pharmaversesdtm)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(stringr)
library(tidyr)
library(tibble)

data("face_vaccine")
data("suppface_vaccine")
data("ex_vaccine")
data("suppex_vaccine")
data("vs_vaccine")
data("admiralvaccine_adsl")

face <- convert_blanks_to_na(face_vaccine)
ex <- convert_blanks_to_na(ex_vaccine)
vs <- convert_blanks_to_na(vs_vaccine)
suppface <- convert_blanks_to_na(suppface_vaccine)
suppex <- convert_blanks_to_na(suppex_vaccine)
adsl <- convert_blanks_to_na(admiralvaccine_adsl)
```

### Pre-processing of Input Dataset

This step involves company-specific pre-processing of required input
dataset for further analysis. In this step, we will filter records that
has only reactogenicity events and combine the `face` and `ex` with
their supplementary datasets `suppface` and `suppex` respectively.

``` r
face <- face %>%
  filter(FACAT == "REACTOGENICITY" & grepl("ADMIN|SYS", FASCAT)) %>%
  mutate(FAOBJ = str_to_upper(FAOBJ)) %>%
  metatools::combine_supp(suppface)
ex <- metatools::combine_supp(ex, suppex)
```

### Merge `FACE` with `EX`

In this step, we will merge `face` with `ex` domain and add required
variables from `ex` domain to the input dataset. If subjects have
multiple vaccination at same visit then this function will not merge
input dataset with `ex` dataset and throws a warning.

The function
[`derive_vars_merged_vaccine()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_merged_vaccine.md)
is used to merge `face` with `ex` domain.

``` r
adface <- derive_vars_merged_vaccine(
  dataset = face,
  dataset_ex = ex,
  by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
  by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
  ex_vars = exprs(EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC, VISIT, VISITNUM)
)
```

This call would return the input dataset with columns from `ex_vars`
added if the subjects does not have multiple vaccination at same visit.

Though the function will throw warning if subjects have multiple
vaccination at same visit, this call would return the input dataset
merging it with supplementary dataset.

### Merge Required `ADSL` Variables Needed for Analysis

At this step, it may be useful to join `ADSL` to your `face` domain.
Only the `ADSL` variables used for derivations are selected at this
step. The rest of the relevant `ADSL` variables would be added later.

``` r
adsl_vars <- exprs(RFSTDTC, RFENDTC)

adface <- derive_vars_merged(
  face,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = get_admiral_option("subject_keys")
)
```

This call would return the input dataset with columns `RFSTDTC`,
`RFENDTC` added.

### Derive Fever Records from `VS` Domain

In this step, we will merge fever records from the `VS` domain with the
input dataset if the fever records does not present in the input
dataset.

The function
[`derive_fever_records()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_fever_records.md)
is used to merge fever records. These records will also be used in
maximum temperature calculation.

``` r
adface <- derive_fever_records(
  dataset = adface,
  dataset_source = ungroup(vs),
  filter_source = VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP",
  faobj = "FEVER"
)
```

This call returns the input dataset with `FEVER` records added if the
input dataset does not have `FEVER` records. If the input dataset has
`FEVER` records, the output dataset will be same as the input dataset.

### Derive/Impute Numeric Date/Time and Analysis Day (`ADT`, `ADTM`, `ADTF`, `ATMF`, `ADY`)

The function
[`derive_vars_dt()`](https://pharmaverse.github.io/admiral/cran-release/reference/derive_vars_dt.html)
can be used to derive `ADT`. This function allows the user to impute the
date as well.

Similarly, `ADTM` can be created using the function
[`derive_vars_dtm()`](https://pharmaverse.github.io/admiral/cran-release/reference/derive_vars_dtm.html).
Imputation can be done on both the date and time components of `ADTM`.

Example calls:

``` r
adface <- adface %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = FADTC
  ) %>%
  derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = FADTC,
    highest_imputation = "n",
    ignore_seconds_flag = FALSE
  )
```

Once `ADT` is derived, the function
[`derive_vars_dy()`](https://pharmaverse.github.io/admiral/cran-release/reference/derive_vars_dy.html)
can be used to derive `ADY`. This example assumes both `ADT` and
`RFSTDTC` exist on the data frame.

``` r
adface <- adface %>%
  mutate(RFSTDTC = as.Date(RFSTDTC)) %>%
  derive_vars_dy(reference_date = RFSTDTC, source_vars = exprs(ADT))
```

### Derive Period Variables (e.g. `APxxSDT`, `APxxEDT`, …)

The [admiral](https://pharmaverse.github.io/admiral/) core package has
separate functions to handle period variables since these variables are
study specific.

See the [“Visit and Period Variables”
vignette](https://pharmaverse.github.io/admiral/articles/visits_periods.html)
for more information.

If the variables are not derived based on a period reference dataset,
they may be derived at a later point of the flow. For example, phases
like “Treatment Phase” and “Follow up” could be derived based on
treatment start and end date.

``` r
period_ref <- create_period_dataset(
  dataset = adsl,
  new_vars = exprs(
    APERSDT = APxxSDT, APEREDT = APxxEDT, TRTA = TRTxxA,
    TRTP = TRTxxP
  )
)

adface <- derive_vars_joined(
  adface,
  dataset_add = period_ref,
  by_vars = get_admiral_option("subject_keys"),
  filter_join = ADT >= APERSDT & ADT <= APEREDT,
  join_type = "all"
)
```

### Derive Direct Mapping Variables

In this step,we will create the user defined function to assign `AVAL`
values from `AVALC` which will be used in further steps.

The user defined functions would look like the following:

``` r
sev_to_numeric <- function(x, y) {
  case_when(
    x == "NONE" ~ 0,
    x == "MILD" ~ 1,
    x == "MODERATE" ~ 2,
    x == "SEVERE" ~ 3,
    TRUE ~ y
  )
}
```

The mapping of these variables is left to the User. An example mapping
may be:

``` r
adface <- adface %>%
  mutate(
    AVALC = as.character(FASTRESC),
    AVAL = suppressWarnings(as.numeric(FASTRESN)),
    AVAL = sev_to_numeric(AVALC, AVAL),
    ATPTREF = FATPTREF,
    ATPT = FATPT,
    ATPTN = FATPTNUM
  )
```

### Derive AN01FL

Creating ANL01FL which would flag the records that will be considered
for analysis and if there is any Investigator and Subject record for the
same day, it would flag the Investigator record over the subject record.

Note: Please, consider which assessment is needed for your analysis. If
you want to prioritize Investigator assessment, please proceed as
follows. Otherwise, change FAEVAL order.

``` r
adface <- adface %>% derive_var_extreme_flag(
  by = exprs(STUDYID, USUBJID, FATPTREF, FAOBJ, FATESTCD, FATPTNUM),
  order = exprs(STUDYID, USUBJID, FATPTREF, FAOBJ, FATESTCD, FATPTNUM, FAEVAL),
  new_var = ANL01FL,
  mode = "first",
  true_value = "Y",
  false_value = NA_character_
)
```

### Derive Severity Records for Administration Site Events

The function
[`derive_diam_to_sev_records()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_diam_to_sev_records.md)
is used to derive the severity records from the diameter records for an
event.

The severity records created will be useful for calculating the maximum
severity.

``` r
adface <- derive_diam_to_sev_records(
  dataset = adface,
  filter_add = ANL01FL == "Y",
  diam_code = "DIAMETER",
  faobj_values = c("REDNESS", "SWELLING"),
  testcd_sev = "SEV",
  test_sev = "Severity/Intensity",
  none = 0,
  mild = 2,
  mod = 5,
  sev = 10
)
```

This call returns the input dataset with severity records derived from
the diameter records for an event.

By default, we will populate `SEV` and `Severity/Intensity` as the
`FATESTCD` and `FATEST` for the newly added records. The function allows
the user to change the `FATESTCD` and `FATEST` as well.

### Derive Maximum Records

In this step, we will derive maximum records for severity, diameter,
temperature using the function
[`derive_extreme_records()`](https://pharmaverse.github.io/admiral/cran-release/reference/derive_extreme_records.html).

``` r
adface <- derive_extreme_records(
  dataset = adface,
  dataset_add = adface,
  filter_add = FATESTCD == "SEV" & ANL01FL == "Y",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  order = exprs(AVAL),
  check_type = "none",
  mode = "last",
  set_values_to = exprs(
    FATEST = "Maximum Severity",
    FATESTCD = "MAXSEV"
  )
)

adface <- derive_extreme_records(
  dataset = adface,
  dataset_add = adface,
  filter_add = FAOBJ %in% c("REDNESS", "SWELLING") & FATESTCD == "DIAMETER" & ANL01FL == "Y",
  by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
  order = exprs(AVAL),
  check_type = "none",
  mode = "last",
  set_values_to = exprs(
    FATEST = "Maximum Diameter",
    FATESTCD = "MAXDIAM"
  )
)

adface <- derive_extreme_records(
  dataset = adface,
  dataset_add = adface,
  filter_add = FAOBJ == "FEVER" & ANL01FL == "Y",
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  order = exprs(VSSTRESN),
  check_type = "none",
  mode = "last",
  set_values_to = exprs(
    FATEST = "Maximum Temperature",
    FATESTCD = "MAXTEMP"
  )
)
```

This call returns the input dataset with maximum records added for the
severity, diameter, temperature.

### Assign `PARAMCD`, `PARAM`, `PARAMN`, `PARCAT1`

To assign parameter level values such as `PARAMCD`, `PARAM`, `PARAMN`,
etc., a lookup needs to be created to join to the source data.
`PARCAT1`, `PARCAT2` variables are assigned from `FACAT`, `FASCAT`
variables.

For example, when creating `ADFACE` dataset, a lookup based on the SDTM
`--TESTCD` value can be created:

| `FATESTCD` | `PARAMCD` | `PARAMN` | `FATEST`             |
|------------|-----------|----------|----------------------|
| SEV        | SEVREDN   | 1        | Severity             |
| DIAMETER   | DIARE     | 2        | Diameter             |
| MAXDIAM    | MDIRE     | 3        | Maximum Diameter cm  |
| MAXTEMP    | MAXTEMP   | 4        | Maximum Temperature  |
| MAXSEV     | MAXSWEL   | 5        | Maximum Severity     |
| OCCUR      | OCFEVER   | 6        | Occurrence Indicator |
| OCCUR      | OCERYTH   | 7        | Occurrence Indicator |
| SEV        | SEVPAIN   | 8        | Severity             |
| OCCUR      | OCPAIN    | 9        | Occurrence Indicator |
| OCCUR      | OCSWEL    | 10       | Occurrence Indicator |

This lookup can now be joined to the source data:

``` r
adface <- derive_vars_params(
  dataset = adface,
  lookup_dataset = lookup_dataset,
  merge_vars = exprs(PARAMCD, PARAMN)
)
```

`PARAMCD` will be always derived from lookup dataset whereas `PARAMN`,
`PARAM`, `PARCAT1`, `PARCAT2` can be either derived from lookup dataset
if mentioned in `merge_vars` argument or derived in the function.

### Derive Maximum Severity Flag

The function
[`derive_vars_max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_max_flag.md)
is used to derive flag variable for the maximum values of an event.

`flag1` - Flags the maximum value per subject per event per Vaccination.
`flag2` - Flags the maximum value per subject per event for Overall.

``` r
adface <- derive_vars_max_flag(
  dataset = adface,
  flag1 = "ANL02FL",
  flag2 = "ANL03FL"
)
#> Joining with `by = join_by(STUDYID, DOMAIN, USUBJID, FASEQ, FALNKGRP, PARAM,
#> FALAT, FALNKID, FALOC, FATESTCD, FATEST, FAOBJ, FACAT, FASCAT, FAEVAL, FAORRES,
#> FAORRESU, EPOCH, FADTC, FADY, FATPT, FATPTNUM, FATPTREF, FARFTDTC, FAEVLINT,
#> FAEVINTX, FASTAT, FAREASND, FASTRESC, FASTRESN, FASTRESU, CLTYP, RFSTDTC,
#> RFENDTC, VSSTRESN, ADT, ADTM, ADY, APERIOD, APERSDT, APEREDT, TRTA, TRTP,
#> AVALC, AVAL, ATPTREF, ATPT, ATPTN, ANL01FL, PARAMCD, PARAMN, PARCAT1, PARCAT2)`
#> Joining with `by = join_by(STUDYID, DOMAIN, USUBJID, FASEQ, FALNKGRP, PARAM,
#> FALAT, FALNKID, FALOC, FATESTCD, FATEST, FAOBJ, FACAT, FASCAT, FAEVAL, FAORRES,
#> FAORRESU, EPOCH, FADTC, FADY, FATPT, FATPTNUM, FATPTREF, FARFTDTC, FAEVLINT,
#> FAEVINTX, FASTAT, FAREASND, FASTRESC, FASTRESN, FASTRESU, CLTYP, RFSTDTC,
#> RFENDTC, VSSTRESN, ADT, ADTM, ADY, APERIOD, APERSDT, APEREDT, TRTA, TRTP,
#> AVALC, AVAL, ATPTREF, ATPT, ATPTN, ANL01FL, PARAMCD, PARAMN, PARCAT1, PARCAT2,
#> ANL02FL)`
```

This call would return the input dataset with columns `ANL02FL`,
`ANL03FL` added by default. This function allows the user to change the
name of the new variables created.

### Derive Event Occurrence Flag

The function
[`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md)
is used to derive flag variable for the events that occurred.

`new_var1` - Flags the record if at least one of the event occurred
within the observation period. `new_var2` - Flags the record if the
event is occurred.

``` r
adface <- derive_vars_event_flag(
  dataset = adface,
  by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
  aval_cutoff = 2.5,
  new_var1 = EVENTFL,
  new_var2 = EVENTDFL
)
```

This call would return the input dataset with columns `EVENTFL`,
`EVENTDFL` added by default. This function allows the user to change the
name of the new variables created as well.

### Post-processing of the Dataset

In this step, we will remove values for all the derived records in SDTM
variables.

``` r
adface <- post_process_reacto(
  dataset = adface,
  filter_dataset = FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") |
    (FATESTCD %in% c("OCCUR", "SEV") & FAOBJ %in% c("FEVER", "REDNESS", "SWELLING"))
)
```

### Add ADSL variables

If needed, the other `ADSL` variables can now be added. List of ADSL
variables already merged held in vector `adsl_vars`

``` r
adsl <- adsl %>%
  convert_blanks_to_na() %>%
  filter(!is.na(USUBJID))

adface <- derive_vars_merged(
  dataset = adface,
  dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
  by_vars = get_admiral_option("subject_keys")
)
```

## Example Script

| ADaM   | Sample Code                                                                                       |
|--------|---------------------------------------------------------------------------------------------------|
| ADFACE | [ad_adface.R](https://github.com/pharmaverse/admiralvaccine/blob/main/inst/templates/ad_adface.R) |
