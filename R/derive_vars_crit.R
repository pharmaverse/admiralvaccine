#' Derive Analysis Criterion Evaluation variables
#'
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @description                                                                   +
#' Derive analysis criterion evaluation result variable, paired with character    +
#' and numeric flags.                                                             +
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'
#' @param dataset
#' Input dataset
#'
#' @param new_var
#' Variables to add
#' The analysis criterion evaluation variable's name (i.e., CRIT1)
#' This name is also used in order to create both charcater and numeric
#' flags variables (i.e., CRIT1FL and CRIT1FN).
#' Is possible to give a different name, base on needs (i.e., ANL01)
#'
#' @param label_var
#' Criterion value
#' A text description defining the condition necessary to satisfy the presence
#' of the criterion
#'
#' @param condition
#' Condition for selecting a subset
#' The condition specified in order to select a subset from the input dataset
#' in which the rule is applied.
#'
#' @param criterion
#' Criterion rule
#' The criterion that each selected row satisfies or not.
#' Returns Y or N for character variable and 1 or 0 fro numeric variable
#' if the criterion is met or not, respectively.
#' Returns NA for not selected rows (not taken into account from condition)
#'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @return dataset with criterion variables
#'
#' @author Federico Baratin
#'
#' @examples
#'
#' is2 <- derive_vars_crit(
#'   dataset = is,
#'   new_var = "CRIT1",
#'   label_var = "Titer >= ISLLOQ",
#'   condition = !is.na(AVAL) & !is.na(ISLLOQ),
#'   criterion = AVAL >= ISLLOQ
#' )
#'
################################################################################# ù
#'
derive_vars_crit <- function(dataset, new_var, label_var, condition, criterion) {
  condition <- assert_filter_cond(enquo(condition))
  criterion <- assert_filter_cond(enquo(criterion))

  var_char <- paste0(new_var, "FL")
  var_num <- paste0(new_var, "FN")

  data <- dataset %>%
    mutate(
      `:=`(
        !!var_char,
        case_when(
          !(!!condition) ~ as.character(NA),
          !!criterion & !!condition ~ "Y",
          !(!!criterion) & !!condition ~ "N"
        )
      ),
      `:=`(
        !!var_num,
        case_when(
          !(!!condition) ~ as.numeric(NA),
          !!criterion & !!condition ~ 1,
          !(!!criterion) & !!condition ~ 0
        )
      ),
      `:=`(
        !!new_var,
        case_when(
          !(!!condition) ~ as.character(NA),
          !!criterion & !!condition ~ label_var,
          !(!!criterion) & !!condition ~ label_var
        )
      )
    )

  return(data)
}
