# Creating Maximum Flag

To Flag the maximum records depends on the grouping variables in a flag
variable.

## Usage

``` r
max_flag(dataset, by_vars, fl)
```

## Arguments

- dataset:

  Input dataset

- by_vars:

  By variables which goes to group by, to create the flag. Pass the
  variables inside the exprs().

- fl:

  Flag variable name, Pass it as string.

## Value

Data frame with flag variable which is flagged for the maximum value
records depending on the variables passed in `by_vars` by user.

## Author

Dhivya Kanagaraj

## Examples

``` r
library(tibble)
library(admiral)

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

max_flag(
  dataset = input,
  by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
  fl = "ANL01FL"
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
