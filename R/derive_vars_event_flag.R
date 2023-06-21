#' Adds Flag Variables for an Occurred Event .
#'
#' Creates two flag variables for the event occurred, one for the event occurred
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
#'  The variables to be considered for grouping for creating a new
#'  variable `new_var1`
#'
#' @param aval_cutoff Cutoff value for `AVAL`
#'
#'
#'    For `TESTCD` code list values based on diameter, if `AVAL` is greater than
#'    `aval_cutoff` then the event is considered to have occurred. For example,
#'    if `aval_cutoff` = 2.5 then the subjects with `AVAL` value greater than
#'    2.5 are considered.
#'
#' @param new_var1 Name of the new flag variable 1
#'
#'     A new flag variable will be created with values `Y` or `N`.
#'     If the event is occurred at least once during a observation period for
#'     a subject then the new variable will be flagged as `Y` otherwise `N`.
#'
#' @param new_var2 Name of the new flag variable 2.
#'
#'     A new flag variable will be created with values `Y` or `N`.
#'     If the event is occurred on the particular day then the new variable will
#'     be flagged as `Y` otherwise `N`.
#'
#' @return The dataset with the flag variables added to it.
#'
#' @details
#'
#'  The event is considered to have occurred if `AVAL` is greater than the `aval_cutoff`
#'  or `AVALC` has values `Y`, `MILD`, `MODERATE`, `SEVERE`. In all other cases, the
#'  event is not considered to have occurred.
#'
#'  The names for the new flag variables created will be sponsor specific.
#'
#'  For the `new_var1` it will flag all observations as "Y" within the by group
#'  if the event occurred at least once during observation period. If the event
#'  is not at all occurred during the observation period then all the
#'  observations within by group will be flagged as "N".
#'
#'  For derived maximum records in `FATESTCD` , the `new_var2` will be
#'  set to `NA`.
#'
#'  If both `new_var1` and `new_var2` are `NULL`, this function will return the
#'  input dataset as output dataset.
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
#'   ~USUBJID, ~FAOBJ, ~ATPTREF, ~AVAL, ~AVALC, ~FATEST, ~FATESTCD, ~FASCAT,
#'   "1", "REDNESS", "VAC1", 3.5, "3.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC1", 1.5, "1.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC1", 4.5, "4.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "FATIGUE", "VAC1", 1, "MILD", "Severity", "SEV", "SYSTEMIC",
#'   "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC",
#'   "1", "FATIGUE", "VAC1", 0, "NONE", "Severity", "SEV", "SYSTEMIC",
#'   "1", "FATIGUE", "VAC1", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC",
#'   "1", "REDNESS", "VAC2", 6.5, "6.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC2", 7.5, "7.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC2", 2.5, "2.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "REDNESS", "VAC2", 7.5, "7.5", "Diameter", "DIAMETER", "ADMIN-SITE",
#'   "1", "FATIGUE", "VAC2", 1, "MILD", "Severity", "SEV", "SYSTEMIC",
#'   "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC",
#'   "1", "FATIGUE", "VAC2", 0, "NONE", "Severity", "SEV", "SYSTEMIC",
#'   "1", "FATIGUE", "VAC2", 2, "MODERATE", "Severity", "SEV", "SYSTEMIC",
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
        `:=`(
          !!new_var1,
          case_when(
            any(AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")) ~ "Y",
            AVAL <= aval_cutoff | AVALC == "N" ~ "N"
          )
        )
      )
  } else if (is.null(new_var1) && !is.null(new_var2)) {
    # Derive only `new_var2`
    data_flag <- dataset %>%
      mutate(
        `:=`(
          !!new_var2,
          case_when(
            AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE") ~ "Y",
            AVAL <= aval_cutoff | AVALC == "N" ~ "N"
          )
        )
      )
  } else if (!is.null(new_var1) && !is.null(new_var2)) {
    # Derive both `new_var1` and `new_var2`
    data_flag <- dataset %>%
      group_by(!!!by_vars) %>%
      mutate(
        `:=`(
          !!new_var1,
          case_when(
            any(AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE")) ~ "Y",
            AVAL <= aval_cutoff | AVALC == "N" ~ "N"
          )
        )
      ) %>%
      ungroup() %>%
      mutate(
        `:=`(
          !!new_var2,
          case_when(
            AVAL > aval_cutoff | AVALC %in% c("Y", "MILD", "MODERATE", "SEVERE") ~ "Y",
            AVAL <= aval_cutoff | AVALC == "N" ~ "N"
          )
        )
      )
  } else {
    data_flag <- dataset
  }
  # Flag derived maximum records in `FATESTCD` as `NA` for `new_var2`
  if (!is.null(new_var2)) {
    data_flag <- data_flag %>%
      mutate(
        !!new_var2 := if_else(grepl("MAX", FATESTCD), NA_character_, !!new_var2)
      )
  }
  data_flag <- convert_blanks_to_na(data_flag)
  return(data_flag)
}
