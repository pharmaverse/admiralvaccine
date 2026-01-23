# Add Vaccination Date Variables to the Output Dataset

Creates vaccination date variables from `EX` domain. A date variable
will be created for each vaccination taking values from the variable
`EXSTDTC`.

## Usage

``` r
derive_vars_vaxdt(dataset, dataset_adsl, by_vars, order)
```

## Arguments

- dataset:

  Input dataset

  The variables specified by the `by_vars` argument are expected.

- dataset_adsl:

  Input adsl dataset

  The vaccination date variables created will be merged with this adsl
  dataset.

- by_vars:

  Grouping variables.

  The variables to be grouped to filter the first observation within
  each by group.

- order:

  Sorting variables.

  The variables order to be specified either in ascending or descending
  order. By default ascending order will be applicable.

## Value

The adsl dataset with vaccination date variables added to it.

## Details

If there are multiple vaccinations for a visit per subject,warning will
be provided and only first observation will be filtered based on the
variable order specified on the `order` argument. In this case, user
need to select the `by_vars` appropriately.

The number of variables created will be based on the number of
vaccinations per subject per visit.

## See also

Other der_var:
[`derive_var_aval_adis()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_var_aval_adis.md),
[`derive_vars_crit()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_crit.md),
[`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md),
[`derive_vars_max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_max_flag.md),
[`derive_vars_merged_vaccine()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_merged_vaccine.md),
[`derive_vars_params()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_params.md)

## Author

Vikram S

## Examples

``` r
library(tibble)
library(admiral)
library(dplyr)

input <- tribble(
  ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXLNKGRP, ~VISIT,
  "A001", "2015-01-10", 1, "DRUG A", "VAC 1", "VISIT 1",
  "A001", "2015-01-11", 2, "DRUG A", "VAC 2", "VISIT 2",
  "A001", "2015-01-12", 3, "DRUG B", "VAC 3", "VISIT 3",
  "A002", "2015-01-13", 1, "DRUG B", "VAC 1", "VISIT 1",
  "A002", "2015-01-14", 2, "DRUG C", "VAC 2", "VISIT 2"
)

adsl <- tribble(
  ~USUBJID, ~SEX, ~AGE,
  "A001", "MALE", 23,
  "A002", "FEMALE", 26,
)

derive_vars_vaxdt(
  dataset = input,
  dataset_adsl = adsl,
  by_vars = exprs(USUBJID, VISITNUM),
  order = exprs(USUBJID, VISITNUM, VISIT, EXSTDTC)
)
#> # A tibble: 2 × 6
#>   USUBJID SEX      AGE VAX01DT    VAX02DT    VAX03DT   
#>   <chr>   <chr>  <dbl> <date>     <date>     <date>    
#> 1 A001    MALE      23 2015-01-10 2015-01-11 2015-01-12
#> 2 A002    FEMALE    26 2015-01-13 2015-01-14 NA        
```
