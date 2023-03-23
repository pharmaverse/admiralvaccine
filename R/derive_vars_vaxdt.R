#' Adds vaccination date variables to the output dataset.
#'
#' Creates vaccination date variables from ex domain.A date variable will be
#' created for each vaccination taking values from the variable `EXSTDTC`.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `by_vars` argument are expected.
#'
#' @param by_vars Grouping variables.
#'
#' *Default: exprs(USUBJID, VISITNUM)*
#'
#' The variables to be grouped to filter the first observation within each
#' by group.
#'
#' @param order Sorting variables.
#'
#' *Default: exprs(USUBJID,VISITNUM,VISIT,EXSTDTC)*
#'
#'  The variables order to be specified either in ascending or descending order.
#'  By default ascending order will be applicable.
#'
#' @return the dataset with vaccination date variables which is created
#' using `EXSTDTC`.
#'
#' @author Vikram S
#'
#' @details
#'
#' If there are multiple vaccinations for a visit per subject,only first
#' observation will be filtered based on the variable order specified
#' on the `order` argument.
#'
#' The number of variables created will be based on the number of vaccinations
#' per subject per visit.
#'
#' The output dataset will have one record per subject.
#'
#' @export
#'
#' @keywords der_adxx
#'
#' @family der_adxx
#'
#' @examples
#' library(tibble)
#' library(admiral)
#' library(dplyr)
#' library(rlang)
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
#'   "ABC001", "MALE", 23,
#'   "ABC002", "FEMALE", 26,
#' )
derive_vars_vaxdt <- function(dataset,
                              dataset_adsl,
                              by_vars,
                              order) {
  # assertion checks
  assert_vars(by_vars)
  assert_order_vars(order, optional = TRUE)
  assert_data_frame(dataset, required_vars = by_vars)
  assert_data_frame(dataset_adsl, required_vars = exprs(USUBJID))

  # derive vaccination date variables
  dt_var <- dataset %>%
    group_by(!!!by_vars) %>%
    arrange(!!!order) %>%
    filter(row_number() == 1) %>%
    pivot_wider(
      names_from = VISITNUM, values_from = EXSTDTC, names_prefix = "vaxdt",
      id_cols = USUBJID
    ) %>%
    ungroup()

  # select only vaccination date variables
  s_name <- dt_var %>% select(starts_with("vaxdt"))

  # rename the derived vaccination date variables
  for (i in seq_along(s_name)) {
    names(s_name)[i] <- paste0("VAX0", i, "DT")
    s_name[[i]] <- as.Date(s_name[[i]])
  }

  # Keep only `USUBJID` and vaccination date variables in the output dataset
  vaxdt_vars <- dt_var %>%
    select(USUBJID) %>%
    bind_cols(s_name)

  adsl <- derive_vars_merged(
    dataset_adsl,
    dataset_add = vaxdt_vars,
    by_vars = exprs(USUBJID)
  )
}
