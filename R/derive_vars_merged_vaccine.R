#' Add New Variable(s) to the Input dataset Based on Variables from Another dataset
#'
#' Add New Variable(s) to the Input dataset Based on Variables from Another dataset.
#' The variables to be added to the output dataset will be based on input variables
#' passed on `ex_vars` argument
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
#'               By default `dataset_supp` will be NULL,user has to provide
#'               supplementary dataset to merge it back with original input dataset
#'               if they have supplementary dataset in their case.
#'
#' @param dataset_suppex Supplementary EX dataset
#'               By default `dataset_suppex` will be NULL,user has to provide
#'               supplementary dataset to merge it back with original EX dataset
#'               if they have supplementary dataset in their case.
#'
#' @param ex_vars Variables to be added to the output dataset from EX dataset
#'
#' @return the dataset with variables added from the EX dataset.
#'
#' @details
#'
#' The input dataset will be merged with Ex dataset for "ADMINISTRATION SITE" and
#' "SYSTEMIC" categories separately and these datasets will be binded together as
#' the final output dataset.
#'
#' The grouping variables are defaulted within the function itself.
#'
#' Only the variables passed to the `ex_vars` will be present in the output dataset
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
#' derive_vars_merged_vaccine(
#'   dataset = vx_face,
#'   dataset_ex = vx_ex,
#'   dataset_supp = NULL,
#'   dataset_suppex = NULL,
#'   ex_vars = exprs(EXTRT, EXDOSE, EXDOSEU, EXSTDTC, EXENDTC)
#' )
#'
derive_vars_merged_vaccine <- function(dataset,
                                       dataset_ex,
                                       dataset_supp = NULL,
                                       dataset_suppex = NULL,
                                       ex_vars) {
  assert_data_frame(dataset)
  assert_data_frame(dataset_supp, optional = TRUE)
  assert_data_frame(dataset_ex, required_vars = NULL)
  assert_data_frame(dataset_suppex, optional = TRUE)


  # combine face and suppface dataset
  if (!is.null(dataset_supp)) {
    dataset <- combine_supp(dataset, dataset_supp)
  }

  # combine face and suppface dataset
  if (!is.null(dataset_suppex)) {
    dataset_ex <- combine_supp(dataset_ex, dataset_suppex)
  }

  # Variables check for FACE

  if (!("FATPTREF" %in% names(dataset))) {
    dataset <- dataset %>%
      mutate(FATPTREF = "VACCINATION 1")
  }

  if (!("FALAT" %in% names(dataset))) {
    dataset <- dataset %>%
      mutate(FALAT = "")
  }

  if (!("FADIR" %in% names(dataset))) {
    dataset <- dataset %>%
      mutate(FADIR = "")
  }

  if (!("FALOC" %in% names(dataset))) {
    dataset <- dataset %>%
      mutate(FALOC = "")
  }

  if (("FALNKGRP" %in% names(dataset))) {
    dataset <- dataset %>%
      mutate(FALNKGRP = str_to_upper(FALNKGRP))
  }

  # Variables check for EX

  if (!("EXTPTREF" %in% names(dataset_ex))) {
    dataset_ex <- dataset_ex %>%
      mutate(EXTPTREF = "VACCINATION 1")
  }

  if (!("EXLAT" %in% names(dataset_ex))) {
    dataset_ex <- dataset_ex %>%
      mutate(EXLAT = "")
  }

  if (!("EXDIR" %in% names(dataset_ex))) {
    dataset_ex <- dataset_ex %>%
      mutate(EXDIR = "")
  }

  if (!("EXLOC" %in% names(dataset_ex))) {
    dataset_ex <- dataset_ex %>%
      mutate(EXLOC = "")
  }

  if (!("EXLNKGRP" %in% names(dataset_ex))) {
    dataset_ex <- dataset_ex %>%
      mutate(EXLNKGRP = EXTPTREF)

    if (!("EXLNKID" %in% names(dataset_ex))) {
      dataset_ex <- dataset_ex %>%
        mutate(EXLNKID = paste0(EXLNKGRP, EXLOC, EXLAT, EXDIR))
    }
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
    # rename EX variables
    dataset_ex <- dataset_ex %>%
      dplyr::rename(FATPTREF = EXLNKGRP, FALOC = EXLOC, FALAT = EXLAT, FADIR = EXDIR)

    # Filter records for  ADMINISTRATION SITE events and merge it with EX dataset
    dataset_adminstration <- dataset %>%
      filter(FASCAT == "ADMINISTRATION SITE")

    face1 <- derive_vars_merged(
      dataset_adminstration,
      dataset_add = dataset_ex,
      new_vars = ex_vars,
      by_vars = exprs(USUBJID, FATPTREF, FALOC, FALAT, FADIR)
    )

    # Filter records for  SYSTEMIC events and merge it with EX dataset
    dataset_systemic <- dataset %>%
      filter(FASCAT == "SYSTEMIC")

    face2 <- derive_vars_merged(
      dataset_systemic,
      dataset_add = dataset_ex,
      new_vars = ex_vars,
      by_vars = exprs(USUBJID, FATPTREF)
    )

    # bind face1 and face2 datasets
    bind_rows(face1, face2)
  }
}
