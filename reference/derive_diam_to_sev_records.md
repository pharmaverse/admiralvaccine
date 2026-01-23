# Creating Severity Records From Diameter

To derive the severity records from the diameter records.

## Usage

``` r
derive_diam_to_sev_records(
  dataset,
  filter_add = NULL,
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

## Arguments

- dataset:

  Input data set

  The variables `USUBJID`,`FAOBJ`,`AVAL`, `AVALC`, `FATESTCD` and
  `FATEST` are expected for Input data set.

- filter_add:

  filter for the `dataset`.

- diam_code:

  Diameter record filter

  *Permitted Value*: A character vector or scalar.

  Helps to filter the diameter records to derive the severity records by
  passing the `FATESTCD` value for diameter which is corresponding to
  the specified events in `faobj_values`.

- faobj_values:

  Event filter

  *Permitted Value*: A character vector or Scalar.

  Helps to filter the events (`Redness` and `Swelling`) which has
  diameter records to derive severity records by passing the events from
  `FAOBJ`.

- testcd_sev:

  To assign `FATESTCD` value for severity

  *Permitted Value*: A character scalar

  Assign the value for `FATESTCD` variable to indicate the severity
  records. Ignore the argument if you want to set the default value
  (`SEV`).

- test_sev:

  `FATEST` Value for severity

  *Permitted Value*: A Character scalar

  Assign the value for `FATEST` variable to indicate the severity
  records. Ignore the argument if you want to set the default value.

- none:

  Pass the lower limit for grade `"NONE"`

  *Permitted Value:* A numeric vector

  The `none` and the following arguments (`mild`, `mode` and `sev`) will
  be used for assigning the diameter limit to derive the `AVALC`
  (severity grade).

  *Assign the lower limit to derive the Severity Grade (`AVALC`).*  
  *For Example: User passing 0 to `none` and 2 to `mild`, 0 will act as
  lower limit and 2 will act* *as upper limit.*  

  *Note: Use the limit reference to pass the values to these
  arguments*  
  *Since the condition was coded like this,*  
  *NONE : `none` \< AVAL \<= `mild`*  
  *MILD : `mild` \< AVAL \<= `mod`*  
  *MODERATE : `mod` \< AVAL \<= `sev`*  
  *SEVERE : `sev` \< AVAL*  
  *User should pass the values as numeric scalar. Refer the default
  values.*

- mild:

  Pass the lower limit for grade `"MILD"`

  *Permitted Value:* A numeric vector

- mod:

  Pass the lower limit for grade `"MODERATE"`

  *Permitted Value:* A numeric vector

- sev:

  Pass the lower limit for grade `"SEVERE"`

  *Permitted Value:* A numeric vector

## Value

The Input data with the new severity records for Redness and swelling
which is specified in `faobj_values` and AVAL, AVALC will be derived and
`FATESTCD`, `FATEST` will be changed as per the values.

## Note

Basically, This function will derive and create the severity records
from the diameter record for the particular events specified in the
`faobj_values` that user wants. If you want to derive the Severity from
diameter, even though you have the severity in SDTM data. This function
will re-derive the severity and remove the derived SDTM severity
records.

## See also

Other der_rec:
[`derive_fever_records()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_fever_records.md)

## Author

Arjun Rubalingam

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(admiral)
library(tibble)

input <- tribble(
  ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
  "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "REDNESS", 1.8, "1.8", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "REDNESS", 1.4, "1.4", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1002", "REDNESS", 11.1, "11.1", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "REDNESS", 7.4, "7.4", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "REDNESS", 6, "6", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "REDNESS", 2.1, "2.1", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "REDNESS", 1.1, "1.1", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1001", "SWELLING", 5.5, "5.5", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "SWELLING", 2.5, "2.5", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "SWELLING", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "SWELLING", 1.8, "1.8", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1001", "SWELLING", 1.4, "1.4", "VACCINATION 1", "Diameter", "DIAMETER",
  "XYZ1002", "SWELLING", 10.1, "10.1", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "SWELLING", 7.1, "7.1", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "SWELLING", 5, "5", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "SWELLING", 1.8, "1.8", "VACCINATION 2", "Diameter", "DIAMETER",
  "XYZ1002", "SWELLING", 1.4, "1.4", "VACCINATION 2", "Diameter", "DIAMETER"
)

derive_diam_to_sev_records(
  dataset = input,
  faobj_values = c("REDNESS", "SWELLING"),
  diam_code = "DIAMETER",
  testcd_sev = "SEV",
  test_sev = "Severity"
)
#> # A tibble: 40 × 8
#>    USUBJID FAOBJ    AVAL AVALC    ATPTREF       FATEST   FATESTCD FASEQ
#>    <chr>   <chr>   <dbl> <chr>    <chr>         <chr>    <chr>    <int>
#>  1 XYZ1001 REDNESS     2 MODERATE VACCINATION 1 Severity SEV         NA
#>  2 XYZ1001 REDNESS     1 MILD     VACCINATION 1 Severity SEV         NA
#>  3 XYZ1001 REDNESS     0 NONE     VACCINATION 1 Severity SEV         NA
#>  4 XYZ1001 REDNESS     0 NONE     VACCINATION 1 Severity SEV         NA
#>  5 XYZ1001 REDNESS     0 NONE     VACCINATION 1 Severity SEV         NA
#>  6 XYZ1002 REDNESS     3 SEVERE   VACCINATION 2 Severity SEV         NA
#>  7 XYZ1002 REDNESS     2 MODERATE VACCINATION 2 Severity SEV         NA
#>  8 XYZ1002 REDNESS     2 MODERATE VACCINATION 2 Severity SEV         NA
#>  9 XYZ1002 REDNESS     1 MILD     VACCINATION 2 Severity SEV         NA
#> 10 XYZ1002 REDNESS     0 NONE     VACCINATION 2 Severity SEV         NA
#> # ℹ 30 more rows
```
