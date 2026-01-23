# Creating ANLxxFL Variables To Flag The Maximum Records

Adds Flags variables for maximum record per subject per event for
overall and per vaccination

## Usage

``` r
derive_vars_max_flag(dataset, flag1 = "ANL01FL", flag2 = "ANL02FL")
```

## Arguments

- dataset:

  Input dataset

- flag1:

  Flags the maximum record per subject per event per vaccination.
  *Permitted value: Any variable name as a string or NULL*.

  `NULL` denotes not to create the flag

- flag2:

  Flags the maximum record per subject per event for Overall

  *Permitted value: Any variable name as a string or NULL*.

  `NULL` denotes not to create the flag

## Value

The output dataframe with `ANLxxFL` flags

## Details

This utility flags the maximum record per subject per event per
vaccination/Overall If both parameters `flag1` & `flag2` are passed as
`NULL` then utility will throw error and flags will not be created.

## See also

Other der_var:
[`derive_var_aval_adis()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_var_aval_adis.md),
[`derive_vars_crit()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_crit.md),
[`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md),
[`derive_vars_merged_vaccine()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_merged_vaccine.md),
[`derive_vars_params()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_params.md),
[`derive_vars_vaxdt()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_vaxdt.md)

## Author

Dhivya Kanagaraj

## Examples

``` r
library(dplyr)
library(admiraldev)
library(admiral)
library(tibble)

input <- tribble(
  ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
  "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
  "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
  "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
  "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
  "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
  "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
  "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
  "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT"
)

derive_vars_max_flag(
  dataset = input,
  flag1 = "ANL01FL",
  flag2 = "ANL02FL"
)
#> Joining with `by = join_by(USUBJID, FAOBJ, FATESTCD, FATPTREF, AVAL, FATPT,
#> PARAMCD)`
#> Joining with `by = join_by(USUBJID, FAOBJ, FATESTCD, FATPTREF, AVAL, FATPT,
#> PARAMCD, ANL01FL)`
#> # A tibble: 8 × 9
#>   USUBJID FAOBJ   FATESTCD FATPTREF  AVAL FATPT PARAMCD ANL01FL ANL02FL
#>   <chr>   <chr>   <chr>    <chr>    <dbl> <chr> <chr>   <chr>   <chr>  
#> 1 ABC101  REDNESS DIAMETER VACC 1      10 DAY 1 DIARE   Y       Y      
#> 2 ABC101  REDNESS DIAMETER VACC 1       7 DAY 2 DIARE   NA      NA     
#> 3 ABC101  REDNESS DIAMETER VACC 2       3 DAY 1 DIARE   NA      NA     
#> 4 ABC101  REDNESS DIAMETER VACC 2       8 DAY 2 DIARE   Y       NA     
#> 5 ABC101  FATIQUE SEV      VACC 1       1 DAY 1 SEVFAT  Y       NA     
#> 6 ABC101  FATIQUE SEV      VACC 1       1 DAY 2 SEVFAT  NA      NA     
#> 7 ABC101  FATIQUE SEV      VACC 2       2 DAY 1 SEVFAT  NA      NA     
#> 8 ABC101  FATIQUE SEV      VACC 2       3 DAY 2 SEVFAT  Y       Y      
derive_vars_max_flag(
  dataset = input,
  flag1 = NULL,
  flag2 = "ANL02FL"
)
#> Joining with `by = join_by(USUBJID, FAOBJ, FATESTCD, FATPTREF, AVAL, FATPT,
#> PARAMCD)`
#> # A tibble: 8 × 8
#>   USUBJID FAOBJ   FATESTCD FATPTREF  AVAL FATPT PARAMCD ANL02FL
#>   <chr>   <chr>   <chr>    <chr>    <dbl> <chr> <chr>   <chr>  
#> 1 ABC101  REDNESS DIAMETER VACC 1      10 DAY 1 DIARE   Y      
#> 2 ABC101  REDNESS DIAMETER VACC 1       7 DAY 2 DIARE   NA     
#> 3 ABC101  REDNESS DIAMETER VACC 2       3 DAY 1 DIARE   NA     
#> 4 ABC101  REDNESS DIAMETER VACC 2       8 DAY 2 DIARE   NA     
#> 5 ABC101  FATIQUE SEV      VACC 1       1 DAY 1 SEVFAT  NA     
#> 6 ABC101  FATIQUE SEV      VACC 1       1 DAY 2 SEVFAT  NA     
#> 7 ABC101  FATIQUE SEV      VACC 2       2 DAY 1 SEVFAT  NA     
#> 8 ABC101  FATIQUE SEV      VACC 2       3 DAY 2 SEVFAT  Y      
derive_vars_max_flag(
  dataset = input,
  flag1 = "ANL01FL",
  flag2 = NULL
)
#> Joining with `by = join_by(USUBJID, FAOBJ, FATESTCD, FATPTREF, AVAL, FATPT,
#> PARAMCD)`
#> # A tibble: 8 × 8
#>   USUBJID FAOBJ   FATESTCD FATPTREF  AVAL FATPT PARAMCD ANL01FL
#>   <chr>   <chr>   <chr>    <chr>    <dbl> <chr> <chr>   <chr>  
#> 1 ABC101  REDNESS DIAMETER VACC 1      10 DAY 1 DIARE   Y      
#> 2 ABC101  REDNESS DIAMETER VACC 1       7 DAY 2 DIARE   NA     
#> 3 ABC101  REDNESS DIAMETER VACC 2       3 DAY 1 DIARE   NA     
#> 4 ABC101  REDNESS DIAMETER VACC 2       8 DAY 2 DIARE   Y      
#> 5 ABC101  FATIQUE SEV      VACC 1       1 DAY 1 SEVFAT  Y      
#> 6 ABC101  FATIQUE SEV      VACC 1       1 DAY 2 SEVFAT  NA     
#> 7 ABC101  FATIQUE SEV      VACC 2       2 DAY 1 SEVFAT  NA     
#> 8 ABC101  FATIQUE SEV      VACC 2       3 DAY 2 SEVFAT  Y      
```
