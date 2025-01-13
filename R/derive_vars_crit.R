#' Derive Analysis Criterion Evaluation Variables
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is *deprecated*, please use `admiral::derive_vars_crit_flag()` instead.
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
#' @keywords deprecated
#' @family deprecated
#'
derive_vars_crit <- function(dataset, prefix, crit_label, condition, criterion) {
  deprecate_warn("0.3.0", "derive_vars_crit()", "admiral::derive_vars_crit_flag()")
  admiral::derive_vars_crit_flag(
    dataset = dataset,
    crit_nr = 1,
    condition = !!criterion,
    description = !!crit_label,
    values_yn = TRUE,
    create_numeric_flag = TRUE
  )
}
