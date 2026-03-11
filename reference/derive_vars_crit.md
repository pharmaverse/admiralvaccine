# Derive Analysis Criterion Evaluation Variables

**\[deprecated\]**

This function is *deprecated*, please use
[`admiral::derive_vars_crit_flag()`](https://pharmaverse.github.io/admiral/cran-release/reference/derive_vars_crit_flag.html)
instead.

Derive analysis criterion evaluation result variable, paired with
character and numeric flags. This function allows also the derivation of
a CRIT like variable with a different name (ex: `ANL01FL`), without
generating additional numeric (ex: `ANL01FN`) and character label (ex:
`ANL01`) variables.

## Usage

``` r
derive_vars_crit(dataset, prefix, crit_label, condition, criterion)
```

## Arguments

- dataset:

  Input dataset

- prefix:

  Variables to add

  The analysis criterion evaluation variable's name (i.e., `CRIT1`) This
  name is also used in order to create both character and numeric flags
  variables (i.e., `CRIT1FL` and `CRIT1FN`). If the name does not
  contain CRIT wording, it generates a flag variable (ex: `ANL01FL`)
  whose logic is equals to `CRIT1` variable, without generating
  additional numeric (ex: `ANL01FN`) and character (`ANL01`) variables.

- crit_label:

  Criterion value

  A text description defining the condition necessary to satisfy the
  presence of the criterion

- condition:

  Condition for selecting a subset

  The condition specified in order to select a subset from the input
  dataset in which the rule is applied.

- criterion:

  Criterion rule

  The criterion that each selected row satisfies or not. Returns `Y` or
  `N` for character variable and `1` or `0` for numeric variable if the
  criterion is met or not, respectively. Returns `NA` for not selected
  rows (not taken into account from condition)

## Value

Dataset with criterion variables

## See also

Other der_var:
[`derive_var_aval_adis()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_var_aval_adis.md),
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

input <- tribble(
  ~USUBJID, ~AVISITN, ~ISCAT, ~PARAMCD, ~AVAL, ~ISLLOQ,
  "999999-000001", 10, "IMMUNOLOGY", "J0033VN", 2, 4,
  "999999-000001", 10, "IMMUNOLOGY", "I0019NT", 3, 6,
  "999999-000001", 10, "IMMUNOLOGY", "M0019LN", 4, 4,
  "999999-000001", 10, "IMMUNOLOGY", "R0003MA", 3, 6,
  "999999-000001", 30, "IMMUNOLOGY", "J0033VN", 60, 4,
  "999999-000001", 30, "IMMUNOLOGY", "I0019NT", 567, 6,
  "999999-000001", 30, "IMMUNOLOGY", "M0019LN", 659, 4,
  "999999-000001", 30, "IMMUNOLOGY", "R0003MA", 250, 6,
  "999999-000002", 10, "IMMUNOLOGY", "J0033VN", 2, 4,
  "999999-000002", 10, "IMMUNOLOGY", "I0019NT", 7, 6,
  "999999-000002", 10, "IMMUNOLOGY", "M0019LN", 5, 4,
  "999999-000002", 10, "IMMUNOLOGY", "R0003MA", 3, 6,
  "999999-000002", 30, "IMMUNOLOGY", "J0033VN", 55, 4,
  "999999-000002", 30, "IMMUNOLOGY", "I0019NT", 89, 6,
  "999999-000002", 30, "IMMUNOLOGY", "M0019LN", 990, 4,
  "999999-000002", 30, "IMMUNOLOGY", "R0003MA", 340, 6,
  "999999-000003", 10, "IMMUNOLOGY", "J0033VN", 3, 4,
  "999999-000003", 10, "IMMUNOLOGY", "I0019NT", 6, 6,
  "999999-000003", 10, "IMMUNOLOGY", "M0019LN", 2, 4,
  "999999-000003", 10, "IMMUNOLOGY", "R0003MA", 2, 6,
  "999999-000003", 30, "IMMUNOLOGY", "J0033VN", 45, 4,
  "999999-000003", 30, "IMMUNOLOGY", "I0019NT", 381, 6,
  "999999-000003", 30, "IMMUNOLOGY", "M0019LN", 542, 4,
  "999999-000003", 30, "IMMUNOLOGY", "R0003MA", NA, 6
)


derive_vars_crit(
  dataset = input,
  prefix = "CRIT1",
  crit_label = "Titer >= ISLLOQ",
  condition = !is.na(AVAL) & !is.na(ISLLOQ),
  criterion = AVAL >= ISLLOQ
)
#> Warning: `derive_vars_crit()` was deprecated in admiralvaccine 0.4.0.
#> ℹ Please use `admiral::derive_vars_crit_flag()` instead.
#> ✖ This message will turn into a error at the beginning of 2026.
#> ℹ See admiral's deprecation guidance:
#>   https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation
#> # A tibble: 24 × 9
#>    USUBJID       AVISITN ISCAT      PARAMCD  AVAL ISLLOQ CRIT1FL CRIT1FN CRIT1  
#>    <chr>           <dbl> <chr>      <chr>   <dbl>  <dbl> <chr>     <dbl> <chr>  
#>  1 999999-000001      10 IMMUNOLOGY J0033VN     2      4 N             0 Titer …
#>  2 999999-000001      10 IMMUNOLOGY I0019NT     3      6 N             0 Titer …
#>  3 999999-000001      10 IMMUNOLOGY M0019LN     4      4 Y             1 Titer …
#>  4 999999-000001      10 IMMUNOLOGY R0003MA     3      6 N             0 Titer …
#>  5 999999-000001      30 IMMUNOLOGY J0033VN    60      4 Y             1 Titer …
#>  6 999999-000001      30 IMMUNOLOGY I0019NT   567      6 Y             1 Titer …
#>  7 999999-000001      30 IMMUNOLOGY M0019LN   659      4 Y             1 Titer …
#>  8 999999-000001      30 IMMUNOLOGY R0003MA   250      6 Y             1 Titer …
#>  9 999999-000002      10 IMMUNOLOGY J0033VN     2      4 N             0 Titer …
#> 10 999999-000002      10 IMMUNOLOGY I0019NT     7      6 Y             1 Titer …
#> # ℹ 14 more rows
```
