#' Adds records for the maximum diameter values within each by group to the
#' output dataset.
#'
#' Creates maximum diameter record per subject per event per period.
#' Maximum diameter records will be derived for the Administration and systemic
#' events which has diameter records.
#'
#' @param dataset Input dataset
#'
#'  The variables `USUBJID`, `APTPTREF`, `FAOBJ`, `FASCAT`, `AVAL`,
#'  `FATESTCD` and `FATEST` are required for the input data set.(`dataset`)
#'
#' @param filter Filter condition
#'
#'   The specified condition is applied to the input dataset before deriving the
#'   new parameter, i.e., only observations fulfilling the condition are taken
#'   into account.
#'
#'   `FATESTCD` and `FAOBJ` related to diameter records are expected to be
#'   filtered in this step
#'
#'
#' @param by_vars Grouping variables
#'
#' *Default: exprs(USUBJID, FAOBJ, ATPTREF)*
#'
#'   Variables to consider for generation of groupwise maximum diameter records.
#'   Providing the names of variables in `exprs()` will create a groupwise
#'   summary and generate maximum diam records for the specified groups.
#'
#' @param test_maxdiam Value for `FATEST`
#'
#' *Default*: "Maximum Diameter"
#' *Permitted Value: A Character scalar*
#'
#' Pass a value for `FATEST` which will be indicate the maximum diameter
#' records
#'
#' @param testcd_maxdiam Value for `FATESTCD`
#'
#' *Default*: "MAXDIAM"
#' *Permitted Value: A Character scalar*
#'
#' Pass a value for `FATESTCD` which will be indicate the maximum diameter
#' records.
#'
#' @author Vikram S
#'
#' @return A data frame with derived records for maximum diameter appended
#'  to the original dataset.
#'
#' @details
#'
#' The maximum aval value of diameter within each group is filtered.
#'
#' if more than one record have same maximum values within by group then
#' this function will take the first record from that group.
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
#'   ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT,
#'   "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC2", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC2", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "2", "SWELLING", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "2", "SWELLING", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "2", "SWELLING", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "2", "SWELLING", "VAC2", .5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "2", "SWELLING", "VAC2", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "2", "SWELLING", "VAC2", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE"
#' )
#'
#' # Derive maximum diameter records for redness and swelling.
#'
#' derive_param_maxdiam(
#'   dataset = input,
#'   filter = FAOBJ %in% c("REDNESS", "SWELLING") & FATESTCD == "DIAMETER",
#'   by_vars = exprs(USUBJID, FAOBJ, FALNKGRP),
#'   test_maxdiam = "Maximum Diameter",
#'   testcd_maxdiam = "MAXDIAM"
#' )
derive_param_maxdiam <- function(dataset,
                                 filter = NULL,
                                 by_vars = NULL,
                                 test_maxdiam = "Maximum Diameter",
                                 testcd_maxdiam = "MAXDIAM") {
  # assert check
  assert_vars(by_vars)
  assert_data_frame(dataset, required_vars = by_vars)
  assert_character_scalar(test_maxdiam, optional = FALSE)
  assert_character_scalar(testcd_maxdiam, optional = FALSE)
  filter <- assert_filter_cond(enquo(filter), optional = TRUE)

  # retaining variables for summary record

  retain_vars <-
    c(
      "USUBJID",
      "FAOBJ",
      "ATPTREF",
      "FALNKGRP",
      "STUDYID",
      "SRCDOM",
      "EXDOSE",
      "EXDOSU",
      "EXSTDTC",
      "EXENDTC",
      "EXTRT",
      "AVAL",
      "FATEST",
      "FATESTCD",
      "DTYPE",
      "AVISIT",
      "AVISITN",
      "FASCAT",
      "FACAT"
    )

  # derive maximum diameter record for each group
  maxdiam <- dataset %>%
    filter(!!filter) %>%
    group_by(!!!by_vars) %>%
    arrange(!!!by_vars, desc(AVAL)) %>%
    filter(row_number() == 1) %>%
    select(any_of(retain_vars)) %>%
    mutate(
      DTYPE = "MAXIMUM",
      FATESTCD = testcd_maxdiam,
      FATEST = test_maxdiam
    )

  # append maximum diameter records with source data
  bind_rows(dataset, maxdiam)
}
