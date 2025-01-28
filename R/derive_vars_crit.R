#' Derive Analysis Criterion Evaluation Variables
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is *deprecated*, please use `admiral::derive_vars_crit_flag()` instead.
#'
#' Derive analysis criterion evaluation result variable, paired with character
#' and numeric flags.
#' This function allows also the derivation of a CRIT like variable with a
#' different name (ex: `ANL01FL`), without generating additional numeric (ex: `ANL01FN`)
#' and character label (ex: `ANL01`) variables.
#'
#'
#' @param dataset Input dataset
#'
#'
#' @param prefix Variables to add
#'
#'   The analysis criterion evaluation variable's name (i.e., `CRIT1`)
#'   This name is also used in order to create both character and numeric
#'   flags variables (i.e., `CRIT1FL` and `CRIT1FN`).
#'   If the name does not contain CRIT wording, it generates a flag variable
#'   (ex: `ANL01FL`) whose logic is equals to `CRIT1` variable, without generating
#'   additional numeric (ex: `ANL01FN`) and character (`ANL01`) variables.
#'
#'
#' @param crit_label Criterion value
#'
#'   A text description defining the condition necessary to satisfy the presence
#'   of the criterion
#'
#'
#' @param condition Condition for selecting a subset
#'
#'   The condition specified in order to select a subset from the input dataset
#'   in which the rule is applied.
#'
#'
#' @param criterion Criterion rule
#'
#'   The criterion that each selected row satisfies or not.
#'   Returns `Y` or `N` for character variable and `1` or `0` for numeric variable
#'   if the criterion is met or not, respectively.
#'   Returns `NA` for not selected rows (not taken into account from condition)
#'
#'
#' @return Dataset with criterion variables
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
#'
#' input <- tribble(
#'   ~USUBJID, ~AVISITN, ~ISCAT, ~PARAMCD, ~AVAL, ~ISLLOQ,
#'   "999999-000001", 10, "IMMUNOLOGY", "J0033VN", 2, 4,
#'   "999999-000001", 10, "IMMUNOLOGY", "I0019NT", 3, 6,
#'   "999999-000001", 10, "IMMUNOLOGY", "M0019LN", 4, 4,
#'   "999999-000001", 10, "IMMUNOLOGY", "R0003MA", 3, 6,
#'   "999999-000001", 30, "IMMUNOLOGY", "J0033VN", 60, 4,
#'   "999999-000001", 30, "IMMUNOLOGY", "I0019NT", 567, 6,
#'   "999999-000001", 30, "IMMUNOLOGY", "M0019LN", 659, 4,
#'   "999999-000001", 30, "IMMUNOLOGY", "R0003MA", 250, 6,
#'   "999999-000002", 10, "IMMUNOLOGY", "J0033VN", 2, 4,
#'   "999999-000002", 10, "IMMUNOLOGY", "I0019NT", 7, 6,
#'   "999999-000002", 10, "IMMUNOLOGY", "M0019LN", 5, 4,
#'   "999999-000002", 10, "IMMUNOLOGY", "R0003MA", 3, 6,
#'   "999999-000002", 30, "IMMUNOLOGY", "J0033VN", 55, 4,
#'   "999999-000002", 30, "IMMUNOLOGY", "I0019NT", 89, 6,
#'   "999999-000002", 30, "IMMUNOLOGY", "M0019LN", 990, 4,
#'   "999999-000002", 30, "IMMUNOLOGY", "R0003MA", 340, 6,
#'   "999999-000003", 10, "IMMUNOLOGY", "J0033VN", 3, 4,
#'   "999999-000003", 10, "IMMUNOLOGY", "I0019NT", 6, 6,
#'   "999999-000003", 10, "IMMUNOLOGY", "M0019LN", 2, 4,
#'   "999999-000003", 10, "IMMUNOLOGY", "R0003MA", 2, 6,
#'   "999999-000003", 30, "IMMUNOLOGY", "J0033VN", 45, 4,
#'   "999999-000003", 30, "IMMUNOLOGY", "I0019NT", 381, 6,
#'   "999999-000003", 30, "IMMUNOLOGY", "M0019LN", 542, 4,
#'   "999999-000003", 30, "IMMUNOLOGY", "R0003MA", NA, 6
#' )
#'
#'
#' derive_vars_crit(
#'   dataset = input,
#'   prefix = "CRIT1",
#'   crit_label = "Titer >= ISLLOQ",
#'   condition = !is.na(AVAL) & !is.na(ISLLOQ),
#'   criterion = AVAL >= ISLLOQ
#' )
#'
derive_vars_crit <- function(dataset, prefix, crit_label, condition, criterion) {
  deprecate_warn(
    when = "0.4.0",
    what = "derive_vars_crit()",
    with = "admiral::derive_vars_crit_flag()",
    details = c(
      x = "This message will turn into a error at the beginning of 2026.",
      i = "See admiral's deprecation guidance:
      https://pharmaverse.github.io/admiraldev/dev/articles/programming_strategy.html#deprecation"
    )
  )
  condition <- assert_filter_cond(enquo(condition))
  criterion <- assert_filter_cond(enquo(criterion))

  var_char <- paste0(prefix, "FL")
  var_num <- paste0(prefix, "FN")

  if (grepl("CRIT", prefix)) {
    data <- dataset %>%
      mutate(
        `:=`(
          !!var_char,
          case_when(
            !(!!condition) ~ NA_character_,
            !!criterion & !!condition ~ "Y",
            !(!!criterion) & !!condition ~ "N"
          )
        ),
        `:=`(
          !!var_num,
          case_when(
            !(!!condition) ~ NA_real_,
            !!criterion & !!condition ~ 1,
            !(!!criterion) & !!condition ~ 0
          )
        ),
        `:=`(
          !!prefix,
          case_when(
            !(!!condition) ~ NA_character_,
            !!criterion & !!condition ~ crit_label,
            !(!!criterion) & !!condition ~ crit_label
          )
        )
      )
  } else {
    data <- dataset %>%
      mutate(
        `:=`(
          !!var_char,
          case_when(
            !(!!condition) ~ NA_character_,
            !!criterion & !!condition ~ "Y",
            !(!!criterion) & !!condition ~ "N"
          )
        )
      )
  }

  return(data)
}
