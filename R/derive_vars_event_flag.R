#' Adds Flag Variables for an Occurred Event .
#'
#' Creates two flag variables for the event occurred,one for the event occurred
#' within each by group and one to flag if the event occurred or not
#' for each day.
#'
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the `by_vars` argument are
#'   expected.
#'
#' @param by_vars Grouping variables
#'
#'       *Permitted values:* list of variables created by `exprs()`
#'
#'  The variables to be considered for grouping for creating a new
#'  variable `new_var1`
#'
#' @param aval_cutoff Cutoff value
#'
#'    The cutoff value to decide on whether to consider the event has occurred
#'    or not for the `TESTCD` based on diameter. For example, if the diameter
#'    value is greater than x only we will consider the event occurred for
#'    that record otherwise it will be considered has no event happened for
#'    that record.
#'
#' @param new_var1 Name of the new flag variable 1
#'
#'     A new flag variable will be created with values "Y" or "N".
#'     If the event is occurred at least once during a observation period for
#'     a subject then the new variable will be flagged as Y otherwise N.
#'
#' @param new_var2 Name of the new flag variable 2.
#'
#'     A new flag variable will be created with values "Y" or "N".
#'     If the event is occurred on the particular day then the new variable will
#'     be flagged as Y otherwise N.
#'
#' @return the dataset with the flag variables added to it.
#'
#' @details
#'
#'  The names for the new flag variables created will be sponsor specific.
#'
#'  For the `new_var1` it will flag all observations as "Y" within the by group
#'  if the event occurred at least once during observation period. If the event
#'  is not at all occurred during the observation period then all the
#'  observations within by group will be flagged as "N".
#'
#'  For derived records with `DTYPE` equals "MAXIMUM" , the `new_var2` will be
#'  set to NA.
#'
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
#' library(rlang)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT, ~DTYPE,
#'   "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "",
#'   "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "",
#'   "1", "REDNESS", "VAC1", 1.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE", "",
#'   "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE", "MAXIMUM",
#'   "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "",
#'   "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "",
#'   "1", "FATIGUE", "VAC1", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "",
#'   "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "MAXIMUM",
#'   "1", "REDNESS", "VAC2", 6.5, "6.5", "Diameter", "DIAMETER", "ADMIN-SITE", "",
#'   "1", "REDNESS", "VAC2", 7.5, "7.5", "Diameter", "DIAMETER", "ADMIN-SITE", "",
#'   "1", "REDNESS", "VAC2", 2.5, "2.5", "Diameter", "DIAMETER", "ADMIN-SITE", "",
#'   "1", "REDNESS", "VAC2", 7.5, "7.5", "Diameter", "DIAMETER", "ADMIN-SITE", "MAXIMUM",
#'   "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC", "",
#'   "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "",
#'   "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC", "",
#'   "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC", "MAXIMUM",
#' )
#'
#' derive_vars_event_flag(
#'   dataset = input,
#'   by_vars = exprs(USUBJID, FAOBJ, ATPTREF),
#'   aval_cutoff = 2.5,
#'   new_var1 = EVENTL,
#'   new_var2 = EVENTDL
#' )
#'
derive_vars_event_flag <- function(dataset,
                                   by_vars,
                                   aval_cutoff,
                                   new_var1 = NULL,
                                   new_var2 = NULL) {
  # assertion check
  assert_data_frame(dataset, required_vars = by_vars)
  new_var1 <- assert_symbol(enexpr(new_var1), optional = TRUE)
  new_var2 <- assert_symbol(enexpr(new_var2), optional = TRUE)
  assert_numeric_vector(aval_cutoff)

  dataset <- convert_na_to_blanks(dataset)

  if (!is.null(new_var1) && is.null(new_var2)) {
    # Derive only `new_var1`
    data_flag <- dataset %>%
      group_by(!!!by_vars) %>%
      mutate(
        !!new_var1 := if_else(any(!(is.na(AVAL)) &
          AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")), "Y", "N")
      )
  } else if (is.null(new_var1) && !is.null(new_var2)) {
    # Derive only `new_var2`
    data_flag <- dataset %>%
      mutate(
        !!new_var2 := if_else(!(is.na(AVAL)) &
          AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE"), "Y", "N")
      )
  } else if (!is.null(new_var1) && !is.null(new_var2)) {
    # Derive both `new_var1` and `new_var2`
    data_flag <- dataset %>%
      group_by(!!!by_vars) %>%
      mutate(
        !!new_var1 := ifelse(any(!(is.na(AVAL)) &
          AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")), "Y", "N")
      ) %>%
      ungroup() %>%
      mutate(
        !!new_var2 := ifelse(!(is.na(AVAL)) &
          AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE"), "Y", "N")
      )
  } else {
    data_flag <- dataset
  }

  # Flag maximum records in `DTYPE` as `NA` for new_var2
  if ("MAXIMUM" %in% data_flag$DTYPE) {
    data_flag <- data_flag %>%
      mutate(
        !!new_var2 := if_else(DTYPE == "MAXIMUM", NA_character_, !!new_var2)
      )
  }
  data_flag <- convert_blanks_to_na(data_flag)
}
