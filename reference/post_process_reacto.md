# Post processing function for ADFACE dataset

This is used to do post processing for ADaM reactogenicity dataset, for
the derived SDTM level records, the corresponding values in FA variables
will be `NA`.

## Usage

``` r
post_process_reacto(
  dataset,
  filter_dataset = FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") | (FATESTCD ==
    "OCCUR" & FAOBJ == "FEVER")
)
```

## Arguments

- dataset:

  Input dataset

- filter_dataset:

  Filter condition Conversion of records in FA variables to NA depends
  on this condition.

## Value

The input dataframe with `NA` values in FA variables where the SDTM
records modified for ADaM derivation purpose.

## Author

Arjun Rubalingam

## Examples

``` r
library(dplyr)
library(admiral)
library(tibble)

input <- tribble(
  ~USUBJID, ~FAOBJ, ~FALAT, ~FACAT, ~FASCAT, ~FATPT, ~FATESTCD, ~PARAMCD, ~AVAL,
  "ABC-1001", "FEVER", NA, "REACTO", "SYS", "DAY 1", "MAXTEMP", "MAXTEMP", 39.4,
  "ABC-1001", "VOMITING", NA, "REACTO", "SYS", "DAY 4", "MAXSEV", "MAXVOMIT", 3,
  "ABC-1001", "SWELLING", "LEFT", "REACTO", "ADMIN", "DAY 1", "MAXSEV", "MAXSWEL", 3,
  "ABC-1001", "REDNESS", "LEFT", "REACTO", "ADMIN", "DAY 2", "DIAMATER", "DIARE", 10.3,
  "ABC-1001", "FEVER", "LEFT", "REACTO", "SYS", "DAY 2", "OCCUR", "OCCFEV", NA
)

post_process_reacto(
  dataset = input,
  filter_dataset = FATESTCD %in% c("MAXSEV", "MAXTEMP") |
    (FATESTCD == "OCCUR" & FAOBJ == "FEVER")
)
#> # A tibble: 5 × 9
#>   USUBJID  FAOBJ   FALAT FACAT  FASCAT FATPT FATESTCD PARAMCD   AVAL
#>   <chr>    <chr>   <chr> <chr>  <chr>  <chr> <chr>    <chr>    <dbl>
#> 1 ABC-1001 NA      NA    NA     NA     NA    NA       MAXTEMP   39.4
#> 2 ABC-1001 NA      NA    NA     NA     NA    NA       MAXVOMIT   3  
#> 3 ABC-1001 NA      NA    NA     NA     NA    NA       MAXSWEL    3  
#> 4 ABC-1001 REDNESS LEFT  REACTO ADMIN  DAY 2 DIAMATER DIARE     10.3
#> 5 ABC-1001 NA      NA    NA     NA     NA    NA       OCCFEV    NA  
```
