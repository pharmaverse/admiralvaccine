# admiralvaccine (development version)
# admiralvaccine 0.3.0

## Updates to Templates and Function
- Removed `dataset_supp` and `dataset_suppex` arguments from `derive_vars_merged_vaccine()` as we are not combining the parental with supplementary inside the function, but can be combined in the ADCE, ADFACE and ADIS template using `combine_supp()` from {metatools} as the {metatools} has been removed from dependency.

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

