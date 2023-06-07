#' Derive AVAL variable for ADIS ADaM domain
#'
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @description                                                                   +
#' Derive AVAL variable for Laboratory Immunology Data ADaM domain.               +
#' A common rule has been decided for its derivation, based on ISLLOQ and ISULOQ  +
#' and/or only ISLLOQ presence. Please, refers to arguments description for       +
#' additional details.                                                            +
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'
#' @param dataset
#' Input dataset.
#'
#' @param lower_rule
#' Derivation rule when ISSTRESN value is below ISLLOQ
#'
#' @param middle_rule
#' Derivation rule when ISSTRESN value is greater than ISLLOQ and
#' lower than ISULOQ.
#' If ISULOQ is not present, derivation rule when ISSTRESN is
#' greater than ISLLOQ
#'
#' @param upper_rule
#' Derivation rule when ISSTRESN value is greater than ISULOQ.
#' This is an optional argument since ISULOQ may not be present.
#'
#' @param round
#' Rounding for AVAL variable. An integer argument which specifies
#' the number of decimals displayed.
#'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @return dataset with AVAL variable derived.
#'
#' @export
#'
#' @author Federico Baratin
#' @export
#' @keywords der_var
#' @family der_var
#'
#' @examples
#' library(tibble)
#' library(admiral)
#' library(admiraldev)
#' library(dplyr)
#' library(rlang)
#'
#' input <- tribble(
#'   ~USUBJID, ~AVISITN, ~PARAMCD, ~PARAM, ~ISORRES, ~ISSTRESN, ~ISLLOQ, ~ISULOQ,
#'   "ABC-1001", 10, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
#'   "ABC-1001", 10, "I0019NT", "I0019NT Antibody", "3", 3.0, 4, 200,
#'   "ABC-1001", 10, "M0019LN", "M0019LN Antibody", ">150", NA, 8, 150,
#'   "ABC-1001", 10, "R0003MA", "R0003MA Antibody", "140.5", 140.5, 4, 120,
#'   "ABC-1001", 30, "J0033VN", "J0033VN Antibody", "2", 2.0, 2, 100,
#'   "ABC-1001", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
#'   "ABC-1001", 30, "M0019LN", "M0019LN Antibody", NA, NA, 8, 150,
#'   "ABC-1001", 30, "R0003MA", "R0003MA Antibody", "98.2", 98.2, 4, 120,
#'   "ABC-1001", 10, "J0033VNL", "LOG10 (J0033VN Antibody)", NA, NA, 2, 100,
#'   "ABC-1001", 10, "I0019NTL", "LOG10 (I0019NT Antibody)", "3", 3.0, 4, 200,
#'   "ABC-1001", 10, "M0019LNL", "LOG10 (M0019LN Antibody)", ">150", NA, 8, 150,
#'   "ABC-1001", 10, "R0003MAL", "LOG10 (R0003MA Antibody)", "140.5", 140.5, 4, 120,
#'   "ABC-1001", 30, "J0033VNL", "LOG10 (J0033VN Antibody)", "2", 2.0, 2, 100,
#'   "ABC-1001", 30, "I0019NTL", "LOG10 (I0019NT Antibody)", NA, NA, 4, 200,
#'   "ABC-1001", 30, "M0019LNL", "LOG10 (M0019LN Antibody)", NA, NA, 8, 150,
#'   "ABC-1001", 30, "R0003MAL", "LOG10 (R0003MA Antibody)", "98.2", 98.2, 4, 120,
#'   "ABC-1002", 10, "J0033VN", "J0033VN Antibody", "3", 3.0, 2, 100,
#'   "ABC-1002", 10, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
#'   "ABC-1002", 10, "M0019LN", "M0019LN Antibody", NA, NA, 8, 150,
#'   "ABC-1002", 10, "R0003MA", "R0003MA Antibody", "48.9", 48.9, 4, 120,
#'   "ABC-1002", 30, "J0033VN", "J0033VN Antibody", NA, NA, 2, 100,
#'   "ABC-1002", 30, "I0019NT", "I0019NT Antibody", NA, NA, 4, 200,
#'   "ABC-1002", 30, "M0019LN", "M0019LN Antibody", "5", 5.0, 8, 150,
#'   "ABC-1002", 30, "R0003MA", "R0003MA Antibody", "228.1", 228.1, 4, 120
#' )
#'
#'
#' output <- derive_var_aval_adis(
#'   dataset = input,
#'   lower_rule = ISLLOQ / 2,
#'   middle_rule = ISSTRESN,
#'   upper_rule = ISULOQ,
#'   round = 2
#' )
#################################################################################
#'
derive_var_aval_adis <-
  function(dataset, lower_rule, middle_rule, upper_rule, round) {
    if (!missing(lower_rule) && !missing(middle_rule) && !missing(upper_rule)) {
      data <- dataset %>%
        mutate(
          AVAL = (
            case_when(
              !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ {{ lower_rule }},
              !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ & ISSTRESN < ISULOQ ~
                {{ middle_rule }},
              !is.na(ISSTRESN) & ISSTRESN >= ISULOQ ~ {{ upper_rule }},
              grepl("<", ISORRES) & !is.na(ISORRES) ~ {{ lower_rule }},
              grepl(">", ISORRES) & !is.na(ISORRES) ~ {{ upper_rule }}
            )
          )
        )
    }

    if (!missing(lower_rule) && !missing(middle_rule) && missing(upper_rule)) {
      data <- dataset %>%
        mutate(
          AVAL = (
            case_when(
              !is.na(ISSTRESN) & ISSTRESN < ISLLOQ ~ {{ lower_rule }},
              !is.na(ISSTRESN) & ISSTRESN >= ISLLOQ ~ {{ middle_rule }},
              grepl("<", ISORRES) & !is.na(ISORRES) ~ {{ lower_rule }},
              grepl(">", ISORRES) & !is.na(ISORRES) ~
                as.numeric(gsub("^.*?>", "", ISORRES))
            )
          )
        )
    }

    if (!missing(round)) {
      data <- data %>%
        mutate(
          AVAL = (round(AVAL, round)
          )
        )
    }

    return(data)
  }
