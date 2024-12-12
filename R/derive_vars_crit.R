#' Derive Analysis Criterion Evaluation Variables
#'
#'
#' @description
#' `r derive_vars_crit::admiralvaccine("deprecated")`
#'
#' This function is *deprecated*, please use `derive_vars_crit_flag` instead.
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
#'
derive_vars_crit <- function(dataset, prefix, crit_label, condition, criterion) {
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
