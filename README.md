<!-- Please do not edit the README.md file as it is auto-generated. Only edit the README.Rmd file -->

# Admiral Extension for Vaccines <img src="man/figures/logo.png" align="right" width="200" style="margin-left:50px;"/>

<!-- badges: start -->

[<img src="https://pharmaverse.org/shields/admiralvaccine.svg">](https://pharmaverse.org/)
[![CRAN
status](https://www.r-pkg.org/badges/version/admiralvaccine)](https://CRAN.R-project.org/package=admiralvaccine/)
[![Test
Coverage](https://raw.githubusercontent.com/pharmaverse/admiralvaccine/badges/main/test-coverage.svg)](https://github.com/pharmaverse/admiralvaccine/actions/workflows/code-coverage.yml/)
<!-- badges: end -->

Vaccine extension package for ADaM in R Asset Library `{admiral}`

## Purpose

To provide a complementary (to `{admiral}`) toolbox that enables users
to develop vaccine specific domains.

## Installation

The package is available from CRAN and can be installed by running
`install.packages("admiralvaccine")`.

To install the latest development version of the package directly from
GitHub use the following code:

    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }

    remotes::install_github("pharmaverse/admiraldev", ref = "devel") # This is a required dependency of {admiralvaccine}
    remotes::install_github("pharmaverse/admiral", ref = "devel") # This is a required dependency of {admiralvaccine}
    remotes::install_github("pharmaverse/admiralvaccine", ref = "devel")

### Dependencies

The latest version of the package works with the latest versions of the
packages stated in `DESCRIPTION`.

If a previous version of the package should be used, it is recommended
to use latest version of the dependencies at the point of time when the
previous version of {admiralvaccine} was released.

## Scope

-   Build a toolbox of re-usable functions and utilities to create
    vaccine-specific ADaM datasets in R in a modular manner.
-   All functions are created based upon the ADaM Implementation Guide
    and aim to facilitate the programming of ADaM dataset standards.
-   Initially the package will focus on creating the reactogenicity and
    immunogenicity domain following flat model as per Center for
    Biologics Evaluation and Research (CBER) guidelines.In future we
    will make enhancements as we get updates on Center for Biologics
    Evaluation and Research (CBER).

## Expectations

`{admiralvaccine}` is expected to complement `{admiral}` and provide
functions to help with the creation of the efficacy endpoints required
for vaccine ADaMs.

## References and Documentation

-   Please refer to the [{admiral} References and
    Documentation](https://pharmaverse.github.io/admiral/)

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

-   [Slack](https://pharmaverse.slack.com/) - for informal discussions,
    Q&A and building our user community. If you don’t have access, use
    this
    [link](https://join.slack.com/t/pharmaverse/shared_invite/zt-yv5atkr4-Np2ytJ6W_QKz_4Olo7Jo9A/)
    to join the pharmaverse Slack workspace
-   [GitHub
    Issues](https://github.com/pharmaverse/admiralvaccine/issues/) - for
    direct feedback, enhancement requests or raising bugs

## {admiralvaccine} R Package Collaboration: Pfizer, GSK and J&J

-   Project Lead & Contributor: Neetu Sangari

-   Maintainer: Sukalpo Saha

-   Developers: Vikram S, Arjun Rubalingam, Dhivya Kanagaraj, Federico
    Baratin, Yamini Purna Bollu, Ilse Augustyns, Kalyani Bodicherla

-   Support lead & Reviewer: Ben Straub

-   Other Reviewers: Stefan Bundfuss, Edoardo Mancini

-   Other Contributors: Ross Farrugia, Abdul Khayat, Jayashree V,
    Jagadish Katam, Ankur Jindal, Andrea Pammolli, Daniele Bottigliengo,
    Ranya Ben Hsain, Lee Armishaw, Hilde Delanghe, Marleen Nijs, Mandy
    Peng, Tina Zhai
