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
#' The input data set with new records `FATESTCD = MAXTEMP`indicating the
#' maximum temperature records based on the specified variables in `by_vars`.
#' `DTYPE` will be populated as `MAXIMUM`. `FATEST`will be populated as
#' specified in `test_maxtemp`and `FATESCD` will be populated as specified in
#' `testcd_maxtemp`. Maximum temperature values will be populated in `AVAL`.
#'
#' @export
#'
#' @examples
#'
# input <- tribble(
# ~USUBJID,     ~FAOBJ,    ~ATPTREF,          ~AVALC,    ~ ATPT,   ~VSSTRESN,
# "XYZ1001",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-1",   38.9,
# "XYZ1001",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-2",   39.2,
# "XYZ1001",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-3",   39.2,
# "XYZ1001",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-4",   39.2,
# "XYZ1001",    "FEVER",   "VACCINATION 1",     "N",     "DAY-5",   33.1,
# "XYZ1001",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-6",   36.0,
# "XYZ1001",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-1",   38.9,
# "XYZ1001",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-2",   37.2,
# "XYZ1001",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-3",   37.3,
# "XYZ1001",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-4",   36.2,
# "XYZ1001",    "FEVER",   "VACCINATION 2",     "N",     "DAY-5",   36.1,
# "XYZ1001",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-6",   34.0,
# "XYZ1002",    "FEVER",   "VACCINATION 1",     "N",     "DAY-1",   36.7,
# "XYZ1002",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-2",   38.2,
# "XYZ1002",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-3",   35.5,
# "XYZ1002",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-4",   38.2,
# "XYZ1002",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-5",   37.5,
# "XYZ1002",    "FEVER",   "VACCINATION 1",     "Y",     "DAY-6",   37.2,
# "XYZ1002",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-1",   38.0,
# "XYZ1002",    "FEVER",   "VACCINATION 2",     "N",     "DAY-2",   37.2,
# "XYZ1002",    "FEVER",   "VACCINATION 2",     "N",     "DAY-3",   37.4,
# "XYZ1002",    "FEVER",   "VACCINATION 2",     "Y",     "DAY-4",   38.2,
# "XYZ1002",    "FEVER",   "VACCINATION 2",     "N",     "DAY-5",   36.1,
# "XYZ1002",    "FEVER",   "VACCINATION 2",     "N",     "DAY-6",   36.2
# ) %>%

#' derive_param_maxtemp(
#' dataset = input,
#' filter_faobj = "FEVER",
#' test_maxtemp = "Maximum Temperature",
#' testcd_maxtemp = "MAXTEMP"
#' )
#'
derive_param_maxtemp <- function(
    dataset = NULL,
    filter_faobj = "FEVER",
    by_vars = vars(USUBJID, FAOBJ, ATPTREF),
    test_maxtemp = "Maximum temperatue",
    testcd_maxtemp = "MAXTEMP") {
  # assertion

  admiral::assert_data_frame(dataset, required_vars = vars(
    USUBJID, FAOBJ, VSSTRESN,
    FATEST, FATESTCD, ATPTREF
  ))
  admiral::assert_vars(by_vars, optional = FALSE)
  admiral::assert_character_scalar(testcd_maxtemp, optional = FALSE)
  admiral::assert_character_scalar(test_maxtemp, optional = FALSE)
  admiral::assert_character_scalar(filter_faobj, optional = FALSE)
  # retaining variables for summary record

  retain_vars <- c(
    "USUBJID", "FAOBJ", "ATPTREF", "FALNKGRP", "STUDYID", "SRCDOM",
    "EXDOSE", "EXDOSU", "EXSTDTC", "EXENDTC", "EXTRT", "AVAL", "AVALC",
    "FATEST", "FATESTCD", "DTYPE", "AVISIT", "AVISITN", "FASCAT",
    "FACAT"
  )

  # Deriving maximum temperature

  max_temp <- dataset %>%
    dplyr::filter(FAOBJ == filter_faobj & VSSTRESN > 0) %>%
    dplyr::group_by(!!!by_vars) %>%
    dplyr::filter(VSSTRESN == max(VSSTRESN)) %>%
    dplyr::mutate(
      AVAL = VSSTRESN,
      AVALC = "",
      DTYPE = "MAXIMUM",
      FATEST = test_maxtemp,
      FATESTCD = testcd_maxtemp
    ) %>%
    dplyr::select(any_of(retain_vars)) %>%
    dplyr::distinct(USUBJID, AVAL, ATPTREF, .keep_all = TRUE) %>%
    # binding with input data set

    dplyr::bind_rows(dataset)
  return(as.data.frame(max_temp))
}
