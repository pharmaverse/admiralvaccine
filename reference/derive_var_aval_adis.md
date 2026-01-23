# Derive AVAL variable for ADIS ADaM domain

Derive `AVAL` variable for Laboratory Immunology Data ADaM domain. A
common rule has been decided for its derivation, based on `ISLLOQ`,
`ISULOQ` and `ISORRES` when both `ISLLOQ` and `ISULOQ` are present. If
`ISULOQ` is not present, the variables used are `ISLLOQ` and `ISORRES`.
Please, refers to arguments description for additional details.

## Usage

``` r
derive_var_aval_adis(
  dataset,
  lower_rule,
  middle_rule,
  upper_rule = NULL,
  round = NULL
)
```

## Arguments

- dataset:

  Input dataset.

- lower_rule:

  Derivation rule when `ISSTRESN` value is below `ISLLOQ`. When
  `ISSTRESN` is missing, the inequality in `ISORRES` is checked for the
  derivation.

- middle_rule:

  Derivation rule when `ISSTRESN` value is greater than `ISLLOQ` and
  lower than `ISULOQ`. If `ISULOQ` is not present, derivation rule when
  `ISSTRESN` is greater than `ISLLOQ`. When `ISSTRESN` is missing, the
  inequality in `ISORRES` is checked for the derivation.

- upper_rule:

  Derivation rule when `ISSTRESN` value is greater than `ISULOQ`. This
  is an optional argument since `ISULOQ` may not be present. When
  `ISSTRESN` is missing, the inequality in `ISORRES`. is checked for the
  derivation. Default value is `NULL`.

- round:

  Rounding for `AVAL` variable. An integer argument which specifies the
  number of decimals displayed. Default value is `NULL`.

## Value

Dataset with `AVAL` variable derived.

## See also

Other der_var:
[`derive_vars_crit()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_crit.md),
[`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md),
[`derive_vars_max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_max_flag.md),
[`derive_vars_merged_vaccine()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_merged_vaccine.md),
[`derive_vars_params()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_params.md),
[`derive_vars_vaxdt()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_vaxdt.md)

## Author

Federico Baratin

## Examples

``` r
library(tibble)
library(admiral)
library(admiraldev)
library(dplyr)
library(rlang)

input <- tribble(
  ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ, ~ISULOQ,
  "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
  "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4, 200,
  "ABC-1001", 10, "M0019LN", "M0019LN Antibody", ">150", NA, 8, 150,
  "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4, 120,
  "ABC-1001", 30, "J0033VN", "J0033VN Antibody", "2", 2.0, 2, 100,
  "ABC-1001", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
  "ABC-1001", 30, "M0019LN", "M0019LN Antibody", NA, NA, 8, 150,
  "ABC-1001", 30, "R0003MA", "R0003MA Antibody", "98.2", 98.2, 4, 120,
  "ABC-1001", 10, "J0033VNL", "LOG10 (J0033VN Antibody)", NA, NA, 2, 100,
  "ABC-1001", 10, "I0019NTL", "LOG10 (I0019NT Antibody)", "3", 3.0, 4, 200,
  "ABC-1001", 10, "M0019LNL", "LOG10 (M0019LN Antibody)", ">150", NA, 8, 150,
  "ABC-1001", 10, "R0003MAL", "LOG10 (R0003MA Antibody)", "140.5", 140.5, 4, 120,
  "ABC-1001", 30, "J0033VNL", "LOG10 (J0033VN Antibody)", "2", 2.0, 2, 100,
  "ABC-1001", 30, "I0019NTL", "LOG10 (I0019NT Antibody)", NA, NA, 4, 200,
  "ABC-1001", 30, "M0019LNL", "LOG10 (M0019LN Antibody)", NA, NA, 8, 150,
  "ABC-1001", 30, "R0003MAL", "LOG10 (R0003MA Antibody)", "98.2", 98.2, 4, 120,
  "ABC-1002", 10, "J0033VN", "J0033VN Antibody", "3", 3.0, 2, 100,
  "ABC-1002", 10, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
  "ABC-1002", 10, "M0019LN", "M0019LN Antibody", NA, NA, 8, 150,
  "ABC-1002", 10, "R0003MA", "R0003MA Antibody", "48.9", 48.9, 4, 120,
  "ABC-1002", 30, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
  "ABC-1002", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
  "ABC-1002", 30, "M0019LN", "M0019LN Antibody", "5", 5.0, 8, 150,
  "ABC-1002", 30, "R0003MA", "R0003MA Antibody", "228.1", 228.1, 4, 120
)

derive_var_aval_adis(
  dataset = input,
  lower_rule = ISLLOQ / 2,
  middle_rule = ISSTRESN,
  upper_rule = ISULOQ,
  round = 2
)
#> # A tibble: 24 × 9
#>    USUBJID  AVISITN PARAMCD  PARAM          ISORRES ISSTRESN ISLLOQ ISULOQ  AVAL
#>    <chr>      <dbl> <chr>    <chr>          <chr>      <dbl>  <dbl>  <dbl> <dbl>
#>  1 ABC-1001      10 J0033VN  J0033VN Antib… NA          NA        2    100  NA  
#>  2 ABC-1001      10 I0019NT  I0019NT Antib… 3            3        4    200   2  
#>  3 ABC-1001      10 M0019LN  M0019LN Antib… >150        NA        8    150 150  
#>  4 ABC-1001      10 R0003MA  R0003MA Antib… 140.5      140.       4    120 120  
#>  5 ABC-1001      30 J0033VN  J0033VN Antib… 2            2        2    100   2  
#>  6 ABC-1001      30 I0019NT  I0019NT Antib… NA          NA        4    200  NA  
#>  7 ABC-1001      30 M0019LN  M0019LN Antib… NA          NA        8    150  NA  
#>  8 ABC-1001      30 R0003MA  R0003MA Antib… 98.2        98.2      4    120  98.2
#>  9 ABC-1001      10 J0033VNL LOG10 (J0033V… NA          NA        2    100  NA  
#> 10 ABC-1001      10 I0019NTL LOG10 (I0019N… 3            3        4    200   2  
#> # ℹ 14 more rows
```
