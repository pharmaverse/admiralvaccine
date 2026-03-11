# Get Started

## Introduction

As this is a package extension, if you are new to
[admiral](https://pharmaverse.github.io/admiral/) then the best place to
first start reading would be this [Get
Started](https://pharmaverse.github.io/admiral/articles/admiral.html)
guide. This extension package follows the same main idea and
conventions, and re-uses many functions from
[admiral](https://pharmaverse.github.io/admiral/), so it is important to
thoroughly understand these to be able to use
[admiralvaccine](https://pharmaverse.github.io/admiralvaccine/). s \#
Derivations

The most important functions in
[admiralvaccine](https://pharmaverse.github.io/admiralvaccine/) follow
the same conventions as
[admiral](https://pharmaverse.github.io/admiral/) but are focused to
vaccine-specific needs. Please refer the Reference Page for more
details.

## Input and Output

It is expected that the input dataset is not grouped. Otherwise an error
is issued.

The output dataset is ungrouped. The observations are not ordered in a
dedicated way. In particular, the order of the observations of the input
dataset may not be preserved.

## Starting a Script

For the vaccine ADaM data structures, an overview of the flow and
example function calls for the most common steps are provided by the
following vignettes:

- [Creating
  ADSL](https://pharmaverse.github.io/admiralvaccine/articles/adsl.md)
- [Creating
  ADCE](https://pharmaverse.github.io/admiralvaccine/articles/adce.md)
- [Creating
  ADFACE](https://pharmaverse.github.io/admiralvaccine/articles/adface.md)
- [Creating
  ADIS](https://pharmaverse.github.io/admiralvaccine/articles/adis.md)

[admiralvaccine](https://pharmaverse.github.io/admiralvaccine/) also
provides template R scripts as a starting point. They can be created by
calling
[`use_ad_template()`](https://pharmaverse.github.io/admiral/cran-release/reference/use_ad_template.html)
from {admiral}, e.g.,

``` r
library(admiral)
```

``` r
use_ad_template(
  adam_name = "adce",
  save_path = "./ad_adce.R",
  package = "admiralvaccine"
)
```

A list of all available templates can be obtained by
[`list_all_templates()`](https://pharmaverse.github.io/admiral/cran-release/reference/list_all_templates.html)
from {admiral}:

``` r
list_all_templates(package = "admiralvaccine")
#> Existing ADaM templates in package 'admiralvaccine':
#> • ADCE
#> • ADFACE
#> • ADIS
#> • ADSL
```

## Support

Support is provided via the [admiral Slack
channel](https://pharmaverse.slack.com/).
