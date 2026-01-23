# Package index

## Derivations for Adding Parameters/Records

### ADFACE-specific

Parameter Derivation Functions helpful for building the ADFACE datasets

- [`derive_diam_to_sev_records()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_diam_to_sev_records.md)
  : Creating Severity Records From Diameter
- [`derive_fever_records()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_fever_records.md)
  : Creating Fever Records

## Derivations for Adding Variables

### Functions used for deriving new variables

- [`derive_var_aval_adis()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_var_aval_adis.md)
  : Derive AVAL variable for ADIS ADaM domain
- [`derive_vars_crit()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_crit.md)
  **\[deprecated\]** : Derive Analysis Criterion Evaluation Variables
- [`derive_vars_event_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_event_flag.md)
  : Adds Flag Variables for an Occurred Event .
- [`derive_vars_max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_max_flag.md)
  : Creating ANLxxFL Variables To Flag The Maximum Records
- [`derive_vars_merged_vaccine()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_merged_vaccine.md)
  : Add New Variable(s) to the Input dataset Based on Variables from
  Another dataset
- [`derive_vars_params()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_params.md)
  : Assigning Parameter Variables
- [`derive_vars_vaxdt()`](https://pharmaverse.github.io/admiralvaccine/reference/derive_vars_vaxdt.md)
  : Add Vaccination Date Variables to the Output Dataset

## Helper Utilities

### Utilities used within Derivation functions

- [`max_flag()`](https://pharmaverse.github.io/admiralvaccine/reference/max_flag.md)
  : Creating Maximum Flag

## Other Advanced Functions

### ADFACE-specific

- [`post_process_reacto()`](https://pharmaverse.github.io/admiralvaccine/reference/post_process_reacto.md)
  : Post processing function for ADFACE dataset

## Example Datasets

You can run
[`admiral::use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/use_ad_template.html)
to produce additional datasets

- [`admiralvaccine_adce`](https://pharmaverse.github.io/admiralvaccine/reference/admiralvaccine_adce.md)
  : Clinical Events Analysis Dataset - Vaccine Specific
- [`admiralvaccine_adface`](https://pharmaverse.github.io/admiralvaccine/reference/admiralvaccine_adface.md)
  : Findings About Clinical Events Analysis Dataset - Vaccine Specific
- [`admiralvaccine_adis`](https://pharmaverse.github.io/admiralvaccine/reference/admiralvaccine_adis.md)
  : Immunogenicity Specimen Assessments Analysis Dataset - Vaccine
  Specific
- [`admiralvaccine_adsl`](https://pharmaverse.github.io/admiralvaccine/reference/admiralvaccine_adsl.md)
  : Subject Level Analysis Dataset - Vaccine Specific
