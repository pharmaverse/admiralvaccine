# admiralvaccine (development version)

## New Features
- New vignette “Explore ADaM Templates” added under the “Get Started” section to allow users to peruse the admiral ADaM templates directly from the documentation website. (#282)
- Added light switch option. (#282)


# admiralvaccine 0.6.0

## Documentation

- In the function documentation references to vignettes were updated to meet CRAN requirements. (#272)
- The "Ask AI" widget was added to the bottom right of each page. It enables users to ask questions about `{admiralvaccine}` and the rest of the `{admiral}` ecosystem and receive answers from an LLM. It is trained on the documentation of all `{admiral}` packages and provided by [kapa.ai](https://docs.kapa.ai/kapa-for-open-source). (#271)
- A link to the `{admiral}` ecosystem page was added to the README sidebar and main text. (#270)

<details>
<summary>Developer Notes</summary>
- Updated the `{lintr}` preferences to use the shared `{admiraldev}` configurations. (#266)
</details>

# admiralvaccine 0.5.0

## Note

The Maintainer role has moved from Sukalpo Saha to Arjun Rubalingam. (#256)

## Updates to Templates

- The `ADIS` template was updated so that the datasets are saved in the same way as the other templates. (#260)

# admiralvaccine 0.4.0

## Breaking Changes

- Function `derive_vars_crit()` was deprecated in favor of `admiral::derive_vars_crit_flag()`. (#253)

## Updates to Templates

- Calls to `derive_vars_crit()` within `ADIS` template were replaced with `admiral::derive_vars_crit_flag()`. (#253)

## Updates to Documentation

- Calls to `derive_vars_crit()` within `ADIS` vignette were replaced with `admiral::derive_vars_crit_flag()`. (#253)

# admiralvaccine 0.3.0

## Updates related to CBER requirements for eDiary data

- Updated ADFACE template and vignette as per new SDTM mapping of Investigator assessment for eDiary data, as recommended by CBER. The collection of the Investigator assessment can happen when data reported by the participants was deemed incorrect or the participant did not complete the eDiary. In these two scenarios, the Investigator assessment is collected within the eDiary platform as a separate record from the original data reported by the participant (if this is allowed by eCOA system) or in the study eCRF. This leads to a change in Solicited Adverse Event SDTM structure (FACE and VS). Additional records coming from Investigator assessment are added, which can be identified through an evaluator variable (FAEVAL/VSEVAL). An update in ADFACE has been provided in order to reflect these changes in SDTM. (#243)

## Breaking Changes

- Removed `dataset_supp` and `dataset_suppex` arguments from `derive_vars_merged_vaccine()` as we are not combining parental with supplementary domains inside the function, but can be optionally combined in the ADCE, ADFACE and ADIS templates using `combine_supp()` function from {metatools}. (#246)

- Included an argument `filter_add` in `derive_diam_to_sev_records()` to pass the subset condition to consider the particular diameter record for convert them to severity records. (#243)

## Updates to Templates

- Included a step to derive ANL01FL in ADFACE template, to flag the records which would be considered for analysis purpose. (#243)

- Supplementary domains are now optionally combined with parental domains in ADFACE and ADIS templates. (#246)

## Updates to Documentation

- Included a step to derive ANL01FL in ADFACE vignette, to flag the records which would be considered for analysis purpose. (#243)

- Supplementary domains are now optionally combined with parental domains n ADFACE and ADIS vignettes. (#246)

## Various

- {metatools} was added to 'Suggests'. (#246)

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

