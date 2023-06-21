#' Add New Variable(s) to the Input dataset Based on Variables from Another dataset
#'
#' Add new variables to the input dataset based on variables from another dataset.
#' The variables to be added to the output dataset will be based on input variables
#' passed on `ex_vars` argument.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `by_vars` argument inside the
#'    `derive_vars_merged`are expected.
#'
#' @param dataset_ex EX dataset to merge with the input dataset.
#'
#'   The variables specified by the `ex_vars` argument are expected.
#'
#' @param dataset_supp Supplementary input dataset
#'
#'    By default `dataset_supp` will be `NULL`, user has to provide
#'    supplementary dataset to merge it back with original input dataset
#'    if they have supplementary dataset in their case.
#'
#' @param dataset_suppex Supplementary EX dataset
#'
#'    By default `dataset_suppex` will be `NULL`, user has to provide
#'    supplementary dataset to merge it back with original `EX` dataset
#'    if they have supplementary dataset in their case.
#'
#' @param by_vars_sys Grouping variables for systemic events.
#'
#' @param by_vars_adms Grouping variables for administration site events.
#'
#' @param ex_vars Variables to be added to the output dataset from EX dataset
#'
#' @return The dataset with variables added from the EX dataset.
#'
#' @details
#'
#' The input dataset will be merged with `EX` dataset for "ADMINISTRATION SITE" and
#' "SYSTEMIC" categories separately and these datasets will be bound together as
#' the final output dataset.
#'
#' This function is intended to add only `EX` variables to the input dataset and user
#' is expected to handle if any pre-processing is required.
#'
#' Only the variables passed to the `ex_vars` will be added in the output dataset
#'
#' If the input dataset has multiple vaccination for a subject at same visit
#' then this function will not merge ex dataset and will return only the input
#' dataset merged with its supplementary dataset.
#'
#' @author Vikram S
#'
#' @export
#'
#' @keywords der_var
#'
#' @family der_var
#'
#' @examples
#'
#' library(tibble)
#' library(admiral)
#' library(dplyr)
#'
#' derive_vars_merged_vaccine(
#'   dataset = vx_face,
#'   dataset_ex = vx_ex,
#'   dataset_supp = NULL,
#'   dataset_suppex = NULL,
#'   by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
#'   by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
#'   ex_vars = exprs(EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)
#' ) %>%
#'   select(USUBJID, FATPTREF, FALOC, FALAT, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC) %>%
#'   head(10)
#'
#' derive_vars_merged_vaccine(
#'   dataset = vx_face,
#'   dataset_ex = vx_ex,
#'   dataset_supp = vx_suppface,
#'   dataset_suppex = vx_suppex,
#'   by_vars_sys = exprs(USUBJID, FATPTREF = EXLNKGRP),
#'   by_vars_adms = exprs(USUBJID, FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT),
#'   ex_vars = exprs(EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)
#' ) %>%
#'   filter(CLTYP == "DAIRY") %>%
#'   select(USUBJID, FATPTREF, CLTYP, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)
derive_vars_merged_vaccine <- function(dataset,
                                       dataset_ex,
                                       by_vars_sys,
                                       by_vars_adms,
                                       dataset_supp = NULL,
                                       dataset_suppex = NULL,
                                       ex_vars) {
  assert_data_frame(dataset)
  assert_vars(by_vars_sys)
  assert_vars(by_vars_adms)
  assert_vars(ex_vars)
  assert_data_frame(dataset_supp, optional = TRUE)
  assert_data_frame(dataset_ex)
  assert_data_frame(dataset_suppex, optional = TRUE)

  # combine face and suppface dataset
  if (!is.null(dataset_supp)) {
    dataset <- combine_supp(dataset, dataset_supp)
  }

  # combine face and suppex dataset
  if (!is.null(dataset_suppex)) {
    dataset_ex <- combine_supp(dataset_ex, dataset_suppex)
  }

  if ("VISIT" %in% names(dataset_ex)) {
    ex_distinct <- dataset_ex %>% distinct(USUBJID, VISIT, .keep_all = TRUE)
  } else {
    ex_distinct <- dataset_ex %>% distinct(USUBJID, VISITNUM, .keep_all = TRUE)
  }

  if (nrow(dataset_ex) != nrow(ex_distinct)) {
    warning("Subjects have multiple vaccinations at same visit")

    dataset <- dataset
  } else {
    # Filter records for  ADMINISTRATION SITE events and merge it with EX dataset
    dataset_adminstration <- dataset %>%
      filter(FASCAT == "ADMINISTRATION SITE")

    face1 <- derive_vars_merged(
      dataset_adminstration,
      dataset_add = dataset_ex,
      new_vars = ex_vars,
      by_vars = by_vars_adms
    )

    # Filter records for  SYSTEMIC events and merge it with EX dataset
    dataset_systemic <- dataset %>%
      filter(FASCAT == "SYSTEMIC")

    face2 <- derive_vars_merged(
      dataset_systemic,
      dataset_add = dataset_ex,
      new_vars = ex_vars,
      by_vars = by_vars_sys
    )

    # bind face1 and face2 datasets
    bind_rows(face1, face2)
  }
}
