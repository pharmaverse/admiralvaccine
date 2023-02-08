<!-- Please do not edit the README.md file as it is auto-generated. Only edit the README.Rmd file -->

# Admiral Extension for Vaccines <img src="man/figures/logo.png" align="right" width="200" style="margin-left:50px;"/>

<!-- badges: start -->

[<img src="http://pharmaverse.org/shields/admiralvaccine.svg">](https://pharmaverse.org)
[![CRAN
status](https://www.r-pkg.org/badges/version/admiralvaccine)](https://CRAN.R-project.org/package=admiralvaccine)
[![Test
Coverage](https://raw.githubusercontent.com/pharmaverse/admiralvaccine/badges/main/test-coverage.svg)](https://github.com/pharmaverse/admiralvaccine/actions/workflows/code-coverage.yml)
<!-- badges: end -->

Vaccine extension package for ADaM in R Asset Library `{admiral}`

## Purpose

To provide a complementary (to `{admiral}`) toolbox that enables users
to develop vaccine specific domains.

## Installation

The package is currently on development stage.

## Scope

-   Build a toolbox of re-usable functions and utilities to create
    vaccine-specific ADaM datasets in R in a modular manner.
-   All functions are created based upon the ADaM Implementation Guide
    and aim to facilitate the programming of ADaM dataset standards.
-   Initially the package will focus on creating the reactogenecity and
    immunogenecity domain following flat model as per CBER guidelines.

## Expectations

`{admiralvaccine}` is expected to complement `{admiral}` and provide
functions to help with the creation of the efficacy endpoints required
for vaccine ADaMs.

## References and Documentation

-   Please refer to the [{admiral} References and
    Documentation](https://pharmaverse.github.io/admiral/index.html#references-and-documentation)

## R Versions

Here’s a summary of our strategy for this package related to R versions:

-   R versions for developers and users will follow the same as
    `{admiral}` core package.
-   For development the `devel` branch of `{admiral}` core is used as a
    dependency. For releasing a new `{admiralvaccine}` version it must
    run using the latest released `{admiral}` core version, i.e., `main`
    branch of `{admiral}` core.

## Contact

We use the following for support and communications between user and
developer community:

-   [Slack](https://app.slack.com/client/T028PB489D3/C02M8KN8269) - for
    informal discussions, Q&A and building our user community. If you
    don’t have access, use this
    [link](https://join.slack.com/t/pharmaverse/shared_invite/zt-yv5atkr4-Np2ytJ6W_QKz_4Olo7Jo9A)
    to join the pharmaverse Slack workspace
-   [GitHub Issues](https://github.com/pharmaverse/admiralonco/issues) -
    for direct feedback, enhancement requests or raising bugs
