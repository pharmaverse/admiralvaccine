# Add New Variable(s) to the Input dataset Based on Variables from Another dataset

Add new variables to the input dataset based on variables from another
dataset. The variables to be added to the output dataset will be based
on input variables passed on `ex_vars` argument.

## Usage

``` r
derive_vars_merged_vaccine(
  dataset,
  dataset_ex,
  by_vars_sys,
  by_vars_adms,
  ex_vars
)
```

## Arguments

- dataset:

  Input dataset which should have been combined with the
  supplementary(if exists).

  The variables specified by the `by_vars` argument inside the
  `derive_vars_merged`are expected.

- dataset_ex:

  `ex` dataset(combined with `suppex`) to merge with the input dataset.

  The variables specified by the `ex_vars` argument are expected.

- by_vars_sys:

  Grouping variables for systemic events.

- by_vars_adms:

  Grouping variables for administration site events.

- ex_vars:

  Variables to be added to the output dataset from EX dataset

## Value

The dataset with variables added from the EX dataset.

## Details

The input dataset will be merged with `EX` dataset for "ADMINISTRATION
SITE" and "SYSTEMIC" categories separately and these datasets will be
bound together as the final output dataset.

This function is intended to add only `EX` variables to the input
dataset and user is expected to handle if any pre-processing is
required.

Only the variables passed to the `ex_vars` will be added in the output
dataset

If the input dataset has multiple vaccination for a subject at same
visit then this function will not merge ex dataset and will return the
`dataset`.

## See also

Other der_var:
[`derive_var_aval_adis()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_var_aval_adis.md),
[`derive_vars_crit()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_crit.md),
[`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md),
[`derive_vars_max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_max_flag.md),
[`derive_vars_params()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_params.md),
[`derive_vars_vaxdt()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_vaxdt.md)

## Author

Vikram S

## Examples

``` r
library(tibble)
library(admiral)
library(dplyr)
library(pharmaversesdtm)

derive_vars_merged_vaccine(
  dataset = face_vaccine,
  dataset_ex = ex_vaccine,
  by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
  by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
  ex_vars = exprs(EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)
) %>%
  select(USUBJID, FATPTREF, FALOC, FALAT, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC) %>%
  head(10)
#> # A tibble: 10 × 9
#>    USUBJID  FATPTREF      FALOC        FALAT EXTRT EXDOSE EXDOSU EXSTDTC EXENDTC
#>    <chr>    <chr>         <chr>        <chr> <chr>  <dbl> <chr>  <chr>   <chr>  
#>  1 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  2 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  3 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  4 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  5 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  6 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  7 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  8 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#>  9 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…
#> 10 ABC-1001 VACCINATION 1 DELTOID MUS… LEFT  VACC…      1 SYRIN… 2021-1… 2021-1…

derive_vars_merged_vaccine(
  dataset = face_vaccine,
  dataset_ex = ex_vaccine,
  by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
  by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
  ex_vars = exprs(EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)
)
#> # A tibble: 307 × 35
#>    STUDYID DOMAIN USUBJID  FASEQ FALNKGRP    FALAT FALNKID FALOC FATESTCD FATEST
#>    <chr>   <chr>  <chr>    <int> <chr>       <chr> <chr>   <chr> <chr>    <chr> 
#>  1 ABC     FACE   ABC-1001     8 VACCINATIO… LEFT  VACCIN… DELT… OCCUR    Occur…
#>  2 ABC     FACE   ABC-1001     9 VACCINATIO… LEFT  VACCIN… DELT… OCCUR    Occur…
#>  3 ABC     FACE   ABC-1001    10 VACCINATIO… LEFT  VACCIN… DELT… SEV      Sever…
#>  4 ABC     FACE   ABC-1001    11 VACCINATIO… LEFT  VACCIN… DELT… OCCUR    Occur…
#>  5 ABC     FACE   ABC-1001    12 VACCINATIO… LEFT  VACCIN… DELT… SEV      Sever…
#>  6 ABC     FACE   ABC-1001    13 VACCINATIO… LEFT  VACCIN… DELT… OCCUR    Occur…
#>  7 ABC     FACE   ABC-1001    14 VACCINATIO… LEFT  VACCIN… DELT… SEV      Sever…
#>  8 ABC     FACE   ABC-1001    15 VACCINATIO… LEFT  VACCIN… DELT… OCCUR    Occur…
#>  9 ABC     FACE   ABC-1001    16 VACCINATIO… LEFT  VACCIN… DELT… SEV      Sever…
#> 10 ABC     FACE   ABC-1001    17 VACCINATIO… LEFT  VACCIN… DELT… OCCUR    Occur…
#> # ℹ 297 more rows
#> # ℹ 25 more variables: FAOBJ <chr>, FACAT <chr>, FASCAT <chr>, FAEVAL <chr>,
#> #   FAORRES <chr>, FAORRESU <chr>, EPOCH <chr>, FADTC <chr>, FADY <dbl>,
#> #   FATPT <chr>, FATPTNUM <dbl>, FATPTREF <chr>, FARFTDTC <chr>,
#> #   FAEVLINT <chr>, FAEVINTX <chr>, FASTAT <chr>, FAREASND <chr>,
#> #   FASTRESC <chr>, FASTRESN <dbl>, FASTRESU <chr>, EXTRT <chr>, EXDOSE <dbl>,
#> #   EXDOSU <chr>, EXSTDTC <chr>, EXENDTC <chr>
```
