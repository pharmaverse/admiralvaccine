# admiralvaccine 0.3.0

## ADFACE update after CBER requirements for eDiary data

- Updated ADFACE template as per new SDTM mapping of Investigator assessment for eDiary data, as recommended by CBER. The collection of the Investigator assessment can happen when data reported by the participants was deemed incorrect or the participant did not complete the eDiary. In case of these two scenarios, the Investigator assessment is collected within the eDiary platform as a separate record from the original data reported by the participant (if this is allowed by eCOA system) or in the study eCRF. This leads to a change in Solicited Adverse Event SDTM structure (FACE and VS). Additional records coming from Investigator assessment are added, which can be indentified through an evaluator variable (FAEVAL/VSEVAL). An update in ADFACE has been provided in order to reflect this changes in SDTM. (#243)

## Breaking Changes

- Removed `dataset_supp` and `dataset_suppex` arguments from `derive_vars_merged_vaccine()` as we are not combining the parental with supplementary inside the function, but can be optionally combined in the ADCE, ADFACE and ADIS templates using `combine_supp()` function from {metatools}. (#246)

## Updates to Templates

- Supplementary domains are now optionally combined with parental domain within the template. (#246)

# admiralvaccine 0.2.0

## Updates to Templates

- Modified calls to `derive_vars_joined()` in ADCE, ADFACE and ADIS templates in line with the updates to this function in the new version of the `{admiral}` package. The `join_type` argument is now always specified and populated as `'all'` (#229).

- Modified calls to `derive_extreme_records()` in ADFACE template in line with the updates to this function in the new version of the `{admiral}` package. The `filter` argument is now renamed to `filter_add` and the argument `dataset_add` is now always specified. (#229).

## Updates to Documentation

- Modified calls to `derive_vars_joined()` in ADCE, ADFACE and ADIS vignettes in line with the updates to this function in the new version of the `{admiral}` package. The `join_type` argument is now always specified and populated as `all` (#229).

- Modified calls to `derive_extreme_records()` in ADFACE vignette in line with the updates to this function in the new version of the `{admiral}` package. The `filter` argument is now renamed to `filter_add` and the argument `dataset_add` is now always specified. (#229).

- All dummy SDTM data used in the package, have been replaced with data available in `{pharmaversesdtm}` package (#228).

## Various

- Website now has button/links to Slack channel and GitHub Issues (#225).

- Lee Armishaw was added as a contributor (#225).

# admiralvaccine 0.1.0

## New Features

- Created SDTM dummy data `IS`, `SUPPIS` for `ADIS` (#31, #26)
- Created SDTM dummy data `DM`, `CE`, `EX`, `FACE`, `VS` for `ADCE` AND `ADFACE`.
- Developed new functionalities for `ADFACE`, `ADIS` and `ADIS` domain.
- Added `metatools`, `metacore` packages in `staged_dependencies.yaml` file.
- Created template for `ADSL`.

## Documentation

- Created `ADIS` vignette.
- Created `ADCE` vignette.
- Created `ADSL` vignette.
- Created `ADFACE` vignette.

