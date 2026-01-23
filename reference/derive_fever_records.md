# Creating Fever Records

Creating Fever records from the `VS` SDTM dataset.

## Usage

``` r
derive_fever_records(dataset, dataset_source, filter_source, faobj)
```

## Arguments

- dataset:

  Input Dataset Input dataset is expected to have variables `USUBJID`
  and `FAOBJ`.

- dataset_source:

  Source Dataset - SDTM Vital Sign (`VS`) Source Dataset (VS) is
  expected to have temperature records.

- filter_source:

  Filter condition for Source dataset.

- faobj:

  FAOBJ Value for fever records in output dataset.

## Value

The output dataset contains records with `FATESTCD = "OCCUR"` for
`FAOBJ = FEVER` records.

## Details

Check if `FAOBJ = FEVER` record is present in input dataset, if not then
use `SDTM.VS` to get FEVER records. With temperature values from
`VSSTRESN` we decide if FEVER has occurred or not (`FAORRES = "Y"/"N"`).
Since records are derived, these FEVER records are considered
`DTYPE = "DERIVED"` if `FAOBJ = FEVER` record is present, then input
dataset will be made as output with no further analysis.

The temperature value greater or equal 38° C will be considered as FEVER
records.

## See also

Other der_rec:
[`derive_diam_to_sev_records()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_diam_to_sev_records.md)

## Author

Dhivya Kanagaraj

## Examples

``` r
library(tibble)
library(dplyr)
library(admiraldev)
#> 
#> Attaching package: ‘admiraldev’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     filter_if
library(admiral)

input <- tribble(
  ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT,
  "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",
  "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",
  "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",
  "ABC101", "FATIQUE", "OCCUR", "REACTOGENICITY", "SYSTEMIC", "DAY 3"
)

vs <- tribble(
  ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
  "ABC101", "TEMP", "REACTOGENICITY", 38.3, "C", "DAY 1",
  "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 2",
  "ABC101", "TEMP", "REACTOGENICITY", 36, "C", "DAY 3",
  "ABC101", "TEMP", "REACTOGENICITY", 37, "C", "DAY 4",
  "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 5",
  "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 6",
  "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 7"
)

derive_fever_records(
  dataset = input,
  dataset_source = vs,
  filter_source = VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP",
  faobj = "FEVER"
)
#> # A tibble: 11 × 10
#>    USUBJID FAOBJ    FATESTCD FACAT FASCAT FATPT FATEST FAORRES FASTRESC VSSTRESN
#>    <chr>   <chr>    <chr>    <chr> <chr>  <chr> <chr>  <chr>   <chr>       <dbl>
#>  1 ABC101  REDNESS  SEV      REAC… ADMIN… DAY 1 NA     NA      NA           NA  
#>  2 ABC101  REDNESS  DIAM     REAC… ADMIN… DAY 2 NA     NA      NA           NA  
#>  3 ABC101  VOMITTI… SEV      REAC… SYSTE… DAY 1 NA     NA      NA           NA  
#>  4 ABC101  FATIQUE  OCCUR    REAC… SYSTE… DAY 3 NA     NA      NA           NA  
#>  5 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 1 Occur… Y       Y            38.3
#>  6 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 2 Occur… Y       Y            38  
#>  7 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 3 Occur… N       N            36  
#>  8 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 4 Occur… N       N            37  
#>  9 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 5 Occur… Y       Y            39  
#> 10 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 6 Occur… Y       Y            39  
#> 11 ABC101  FEVER    OCCUR    REAC… SYSTE… DAY 7 Occur… Y       Y            38  
```
