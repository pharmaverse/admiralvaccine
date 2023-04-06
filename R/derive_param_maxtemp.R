#' Deriving maximum temperature record
#'
#' @description
#' Create a summary record for maximum temperature per subject per period.
#'
#' @param dataset Input data set
#'
#'  Input data set should have the temperature record which will be derived by
#'  using the function `derive_param_fever_occur`.
#'
#' @param filter_faobj filter for fever record
#'
#' *Default: "FEVER"*
#' *Permitted Value: A Character Scalar*
#'
#' Helps to filter the fever record by passing the value from `FATESTCD` which
#' indicates the fever records.
#'
#' @param by_vars Grouping variables
#'
#' *Default: vars(USUBJID,FAOBJ,ATPTREF)*
#' *Permitted Value: A Character vector*
#'
#'  Pass the `by_vars` in `vars()` based on the by_vars variables the maximum
#'  temperature will be derived from `VSSTRESN`.
#'
#' @param test_maxtemp Value for `FATEST`
#'
#' *Default*: "Maximum temperature"
#' *Permitted Value: A Character scalar*
#'
#' Pass a value for `FATEST` which will be indicate the maximum temperature
#' records
#'
#' @param testcd_maxtemp
#'
#' *Default*: "MAXTEMP"
#' *Permitted Value: A Character scalar*
#'
#' Pass a value for `FATESTCD` which will be indicate the maximum temperature
#' records. refer the default value.
#'
#' @return
#' The input data set with derived records indicating the maximum temperature
#' records.
#'
#' @export
#'
#' @family der_adxx
#' @keywords der_adxx
#'
#' @examples
#' library(tibble)
#' library(admiral)
#' library(dplyr)
#' library(rlang)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVALC, ~ATPTN, ~VSSTRESN, ~FATEST, ~FATESTCD,
#'   "XYZ1001", "FEVER", "VACC 1", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 1", "Y", 2, 39.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 1", "Y", 3, 39.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 1", "Y", 4, 39.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 1", "N", 5, 33.1, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 1", "Y", 6, 36.0, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 2", "Y", 1, 38.9, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 2", "Y", 2, 37.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 2", "Y", 3, 37.3, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 2", "Y", 4, 36.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 2", "N", 5, 36.1, "Occurrence Indicator", "OCCUR",
#'   "XYZ1001", "FEVER", "VACC 2", "Y", 6, 34.0, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 1", "N", 1, 36.7, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 1", "Y", 2, 38.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 1", "Y", 3, 35.5, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 1", "Y", 4, 38.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 1", "Y", 5, 37.5, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 1", "Y", 6, 37.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 2", "Y", 1, 38.0, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 2", "N", 2, 37.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 2", "N", 3, 37.4, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 2", "Y", 4, 38.2, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 2", "N", 5, 36.1, "Occurrence Indicator", "OCCUR",
#'   "XYZ1002", "FEVER", "VACC 2", "N", 6, 36.2, "Occurrence Indicator", "OCCUR"
#' )
#'
#' derive_param_maxtemp(
#'   dataset = input,
#'   filter_faobj = "FEVER",
#'   test_maxtemp = "Maximum Temperature",
#'   testcd_maxtemp = "MAXTEMP",
#'   by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
#' )
#'
derive_param_maxtemp <- function(dataset = NULL,
                                 filter_faobj = "FEVER",
                                 by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
                                 test_maxtemp = "Maximum temperatue",
                                 testcd_maxtemp = "MAXTEMP") {
  # assertion

  assert_data_frame(dataset, required_vars = exprs(
    USUBJID, FAOBJ, VSSTRESN,
    FATEST, FATESTCD, ATPTREF
  ))
  assert_character_scalar(testcd_maxtemp, optional = FALSE)
  assert_character_scalar(test_maxtemp, optional = FALSE)
  assert_character_scalar(filter_faobj, optional = FALSE)
  # retaining variables for summary record

  retain_vars <- c(
    "USUBJID", "FAOBJ", "ATPTREF", "FALNKGRP", "STUDYID", "SRCDOM",
    "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC", "EXTRT", "AVAL", "AVALC",
    "FATEST", "FATESTCD", "DTYPE", "AVISIT", "AVISITN", "FASCAT",
    "FACAT"
  )


  # Deriving maximum temperature
  if (filter_faobj %in% dataset$FAOBJ) {
    max_temp <- dataset %>%
      filter(FAOBJ == filter_faobj & VSSTRESN > 0) %>%
      group_by(!!!by_vars) %>%
      filter(VSSTRESN == max(VSSTRESN)) %>%
      mutate(
        AVAL = VSSTRESN,
        AVALC = VSSTRESN,
        AVALC = as.character(AVALC),
        DTYPE = "MAXIMUM",
        FATEST = test_maxtemp,
        FATESTCD = testcd_maxtemp
      ) %>%
      select(any_of(retain_vars)) %>%
      distinct(USUBJID, AVAL, ATPTREF, .keep_all = TRUE)
    # binding with input data set

    bind_rows(dataset, max_temp)
  } else {
    stop(
      paste0(filter_faobj, " ", "doesn't exist in the FAOBJ variable.")
    )
  }
}
