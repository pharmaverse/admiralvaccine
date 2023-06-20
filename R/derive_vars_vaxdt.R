#' Add Vaccination Date Variables to the Output Dataset
#'
#' Creates vaccination date variables from `EX` domain. A date variable will be
#' created for each vaccination taking values from the variable `EXSTDTC`.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `by_vars` argument are expected.
#'
#' @param dataset_adsl Input adsl dataset
#'
#'   The vaccination date variables created will be merged with this adsl dataset.
#'
#' @param by_vars Grouping variables.
#'
#' The variables to be grouped to filter the first observation within each
#' by group.
#'
#' @param order Sorting variables.
#'
#'  The variables order to be specified either in ascending or descending order.
#'  By default ascending order will be applicable.
#'
#' @return The adsl dataset with vaccination date variables added to it.
#'
#' @author Vikram S
#'
#' @details
#'
#' If there are multiple vaccinations for a visit per subject,warning will be
#' provided and only first observation will be filtered based on the variable
#' order specified on the `order` argument. In this case, user need to select
#' the `by_vars` appropriately.
#'
#' The number of variables created will be based on the number of vaccinations
#' per subject per visit.
#'
#' @export
#'
#' @keywords der_var
#'
#' @family der_var
#'
#' @examples
#' library(tibble)
#' library(admiral)
#' library(dplyr)
#'
#' input <- tribble(
#'   ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXLNKGRP, ~VISIT,
#'   "A001", "2015-01-10", 1, "DRUG A", "VAC 1", "VISIT 1",
#'   "A001", "2015-01-11", 2, "DRUG A", "VAC 2", "VISIT 2",
#'   "A001", "2015-01-12", 3, "DRUG B", "VAC 3", "VISIT 3",
#'   "A002", "2015-01-13", 1, "DRUG B", "VAC 1", "VISIT 1",
#'   "A002", "2015-01-14", 2, "DRUG C", "VAC 2", "VISIT 2"
#' )
#'
#' adsl <- tribble(
#'   ~USUBJID, ~SEX, ~AGE,
#'   "A001", "MALE", 23,
#'   "A002", "FEMALE", 26,
#' )
#'
#' derive_vars_vaxdt(
#'   dataset = input,
#'   dataset_adsl = adsl,
#'   by_vars = exprs(USUBJID, VISITNUM),
#'   order = exprs(USUBJID, VISITNUM, VISIT, EXSTDTC)
#' )
derive_vars_vaxdt <- function(dataset,
                              dataset_adsl,
                              by_vars,
                              order) {
  # Assertion checks
  assert_vars(by_vars)
  assert_expr_list(order, optional = TRUE)
  assert_data_frame(dataset, required_vars = by_vars)
  assert_data_frame(dataset_adsl, required_vars = exprs(USUBJID))

  if ("VISIT" %in% names(dataset)) {
    ex_distinct <- dataset %>% distinct(USUBJID, VISIT, .keep_all = TRUE)
  } else {
    ex_distinct <- dataset %>% distinct(USUBJID, VISITNUM, .keep_all = TRUE)
  }

  if (nrow(dataset) != nrow(ex_distinct)) {
    warning("Subjects have multiple vaccinations at same visit")
  }
  # Derive vaccination date variables.
  dt_var <- dataset %>%
    group_by(!!!by_vars) %>%
    arrange(!!!order) %>%
    filter(row_number() == 1) %>%
    pivot_wider(
      names_from = VISITNUM, values_from = EXSTDTC, names_prefix = "vaxdt",
      id_cols = USUBJID
    ) %>%
    ungroup()

  # Select only vaccination date variables.
  s_name <- dt_var %>% select(starts_with("vaxdt"))

  # Rename the derived vaccination date variables.
  for (i in seq_along(s_name)) {
    names(s_name)[i] <- paste0("VAX0", i, "DT")
    s_name[[i]] <- as.Date(s_name[[i]])
  }

  # Keep only `USUBJID` and vaccination date variables.
  vaxdt_vars <- dt_var %>%
    select(USUBJID) %>%
    bind_cols(s_name)

  # Add vaccination date variables to the `ADSL` dataset.
  adsl <- derive_vars_merged(
    dataset_adsl,
    dataset_add = vaxdt_vars,
    by_vars = exprs(USUBJID)
  )
  return(adsl)
}
