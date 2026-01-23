# Assigning Parameter Variables

Creating `PARAMCD` from lookup dataset and assigning
`PARAM`,`PARAMN`,`PARCAT1`, `PARCAT2` variables

## Usage

``` r
derive_vars_params(dataset, lookup_dataset, merge_vars)
```

## Arguments

- dataset:

  Input dataset Input dataset is expected to have variables
  `USUBJID`,`FAOBJ`, `FACAT`, `FATESTCD` and `FATEST`

- lookup_dataset:

  lookup dataset containing `PARAMCD` values for every unique `FATESTCD`
  and `FAOBJ` lookup dataset is expected to have variables `FATEST`,
  `PARAMCD`, `FATESTCD`, `FAOBJ` and one entry for every unique
  `FATESTCD` and `FAOBJ`

- merge_vars:

  List of Variables need to be merged from lookup dataset

## Value

The output dataset contains all observations and variables of the input
dataset along with `PARAM`,`PARAMCD`,`PARCAT1`,`PARCAT2`,`PARAMN`

## Details

A lookup dataset is required with `PARAMCD` values for every combination
of `FATEST` & `FAOBJ`. `PARAMCD` `PARAMN` `PARAMN` `PARCAT1` `PARCAT2`
values can be assigned from lookup dataset.

     if `PARAMN` not assigned in lookup dataset then
     `PARAMN` is assigned with a unique number for every unique PARAM value.
     if `PARAM` value not assigned in lookup dataset then
     `PARAM` value is a combination of `FAOBJ` `FATEST` `FASTRESU` `FALOC`
     `FADIR` `FALAT`
     if `PARCAT1` value not assigned in lookup dataset then
     `PARCAT1` is assigned as `FACAT`
     if `PARCAT2` value not assigned in lookup dataset then
     `PARCAT2` is assigned as `FASCAT`

## See also

Other der_var:
[`derive_var_aval_adis()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_var_aval_adis.md),
[`derive_vars_crit()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_crit.md),
[`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md),
[`derive_vars_max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_max_flag.md),
[`derive_vars_merged_vaccine()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_merged_vaccine.md),
[`derive_vars_vaxdt()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_vaxdt.md)

## Author

Dhivya Kanagaraj

## Examples

``` r
library(admiral)
library(tibble)
library(dplyr)

lookup_dataset <- tibble::tribble(
  ~FATESTCD, ~PARAMCD, ~PARAMN, ~FATEST, ~FAOBJ,
  "SEV", "SEVREDN", 1, "Severity", "Redness",
  "DIAMETER", "DIARE", 2, "Diameter", "Redness",
  "MAXDIAM", "MDIRE", 3, "Maximum Diameter cm", "Redness",
  "MAXTEMP", "MAXTEMP", 4, "Maximum Temperature", "Fever",
  "OCCUR", "OCFEVER", 5, "Occurrence Indicator", "Fever",
  "OCCUR", "OCERYTH", 6, "Occurrence Indicator", "Erythema",
  "SEV", "SEVPAIN", 7, "Severity", "Pain at Injection site",
  "OCCUR", "OCPAIN", 8, "Occurrence Indicator", "Pain at Injection site",
  "OCCUR", "OCSWEL", 9, "Occurrence Indicator", "Swelling"
)

input <- tibble::tribble(
  ~USUBJID, ~FACAT, ~FASCAT, ~FATESTCD, ~FAOBJ, ~FATEST, ~FALOC, ~FALAT,
  "ABC101", "REACTO", "ADMIN", "SEV", "Redness", "Severity", "ARM", "LEFT",
  "ABC101", "REACTO", "ADMIN", "DIAMETER", "Redness", "Diameter", "ARM", "RIGHT",
  "ABC101", "REACTO", "ADMIN", "MAXDIAM", "Redness", "Maximum Diameter", NA, NA,
  "ABC101", "REACTO", "SYSTEMIC", "MAXTEMP", "Fever", "Maximum Temp", NA, NA,
  "ABC101", "REACTO", "SYSTEMIC", "OCCUR", "Fever", "Occurrence", NA, NA,
  "ABC101", "REACTO", "ADMIN", "OCCUR", "Erythema", "Occurrence", NA, NA,
  "ABC101", "REACTO", "ADMIN", "SEV", "Swelling", "Severity", NA, NA,
  "ABC101", "REACTO", "ADMIN", "OCCUR", "Swelling", "Occurrence", NA, NA,
  "ABC101", "REACTO", "ADMIN", "OCCUR", "Swelling", "Occurrence", NA, NA
)

derive_vars_params(
  dataset = input,
  lookup_dataset = lookup_dataset,
  merge_vars = exprs(PARAMCD, PARAMN)
)
#> # A tibble: 9 × 13
#>   USUBJID FACAT  FASCAT   FATESTCD PARAM FAOBJ FATEST FALOC FALAT PARAMCD PARAMN
#>   <chr>   <chr>  <chr>    <chr>    <chr> <chr> <chr>  <chr> <chr> <chr>    <dbl>
#> 1 ABC101  REACTO ADMIN    SEV      Redn… Redn… Sever… ARM   LEFT  SEVREDN      1
#> 2 ABC101  REACTO ADMIN    DIAMETER Redn… Redn… Diame… ARM   RIGHT DIARE        2
#> 3 ABC101  REACTO ADMIN    MAXDIAM  Redn… Redn… Maxim… NA    NA    MDIRE        3
#> 4 ABC101  REACTO SYSTEMIC MAXTEMP  Feve… Fever Maxim… NA    NA    MAXTEMP      4
#> 5 ABC101  REACTO SYSTEMIC OCCUR    Feve… Fever Occur… NA    NA    OCFEVER      5
#> 6 ABC101  REACTO ADMIN    OCCUR    Eryt… Eryt… Occur… NA    NA    OCERYTH      6
#> 7 ABC101  REACTO ADMIN    SEV      Swel… Swel… Sever… NA    NA    NA          NA
#> 8 ABC101  REACTO ADMIN    OCCUR    Swel… Swel… Occur… NA    NA    OCSWEL       9
#> 9 ABC101  REACTO ADMIN    OCCUR    Swel… Swel… Occur… NA    NA    OCSWEL       9
#> # ℹ 2 more variables: PARCAT1 <chr>, PARCAT2 <chr>
```
