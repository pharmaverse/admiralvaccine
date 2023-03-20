#' Creating Maximum Severity Records
#'
#' Creating the maximum severity records per subject per event per period.
#' Maximum severity records will be derived for the Administration and systemic
#' events which has severity records `FATESTCD = "SEV"`.
#'
#' @param dataset input data set
#'
#'  The variables `USUBJID`, `APTPTREF`, `FAOBJ`, `FASCAT`, `AVALC`, `FAOBJ`,
#'  `FATESTCD` and `FATEST` are required for the input data set.(`dataset`)
#'
#' @param exclude_events To exclude the events
#'
#'  Helps to exclude the events which user doesn't want to derive the maximum
#'  severity. Pass the event name in quotes which is from `FAOBJ`.
#'
#' *Default: NULL*
#' *Permitted Value*: A character vector.
#'
#' @param filter_sev severity record filter
#'
#' *Default: "SEV"*
#' *Permitted Value*: A character scalar
#'
#' To filter the severity records to derive the maximum severity by passing the
#' `FATESTCD` value for severity in quotes `"SEV"` for this argument.
#'
#' @param test_maxsev FATEST value for maximum severity records
#'
#' *Default: "Maximum severity"*
#' *Permitted Value*: A character scalar
#'
#' Assign the value for `FATEST` variable to indicate the maximum severity
#' records. Ignore the argument if you want to set the value as mentioned in
#' default.
#'
#' @param testcd_maxsev FATESTCD value for maximum severity records
#'
#' *Default: "MAXSEV"*
#' *Permitted Value*: A character scalar
#'
#' Assign the value for `FATESTCD` variable to indicate the maximum severity
#' records. Ignore the argument if you want to set the value as mentioned in
#' default.
#'
#' @param by_vars Grouping variables
#'
#' *Default: exprs(USUBJID, FAOBJ, ATPTREF)*
#'
#' Based on the variables which will be passed on this `by_vars` argument, The
#' maximum severity records will be derived from AVAL.
#' *Note:* Pass the variables in `exprs()`.
#'
#' @return The input data set with new records `FATESTCD = MAXSEV`indicating the
#' maximum severity records for the specified variables in `by_vars`. `DTYPE`
#' will be populated as `MAXIMUM`. `FATEST`will be populated as specified in
#' `test_maxsev`and `FATESCD` will be populated as specified in `testcd_maxsev`.
#'
#' @author Arjun Rubalingam
#'
#' @export
#'
#' @keywords der_adxx
#' @family der_adxx
#'
#' @examples
#' library(tibble)
#' library(admiral)
#' library(dplyr)
#' library(rlang)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD, ~FASCAT,
#'   "XYZ1001", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 2, "MODERATE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 3, "SEVERE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 2, "MILD", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 2, "MODERATE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 1, "MILD", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 1, "MILD", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 0, "NONE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 1, "MILD", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 0, "NONE", "VACC1", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 2, "MODERATE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 1, "MILD", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 3, "SEVERE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1002", "REDNESS", 0, "NONE", "VACC2", "Severity", "SEV", "ADMIN-SITE",
#'   "XYZ1001", "CHILLS", NA, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1001", "CHILLS", NA, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "MODERATE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "MILD", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "NONE", "VACC1", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "MILD", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "MODERATE", "VACC2", "Severity", "SEV", "SYSTEMIC",
#'   "XYZ1002", "CHILLS", NA, "NONE", "VACC2", "Severity", "SEV", "SYSTEMIC"
#' )
#'
#' derive_param_maxsev(
#'   dataset = input,
#'   filter_sev = "SEV",
#'   exclude_events = "REDNESS",
#'   test_maxsev = "Maximum severity",
#'   testcd_maxsev = "MAXSEV",
#'   by_vars = exprs(USUBJID, FAOBJ, ATPTREF)
#' )
#'
derive_param_maxsev <- function(dataset = NULL,
                                exclude_events = NULL,
                                filter_sev = "SEV",
                                test_maxsev = "Maximum Severity",
                                testcd_maxsev = "MAXSEV",
                                by_vars = exprs(USUBJID, FAOBJ, ATPTREF)) {
  # assertions
  assert_data_frame(dataset,
    required_vars = exprs(
      USUBJID, FASCAT, AVALC, FAOBJ,
      ATPTREF, FATEST, FATESTCD
    )
  )
  assert_character_scalar(filter_sev, optional = FALSE)
  assert_character_vector(exclude_events, optional = TRUE)
  assert_character_scalar(test_maxsev, optional = FALSE)
  assert_character_scalar(testcd_maxsev, optional = FALSE)

  # pre-processing
  if (filter_sev %in% dataset$FATESTCD) {
    maxsev_pp <- dataset %>%
      filter(FATESTCD == filter_sev &
        grepl("ADMIN|SYS", FASCAT)) %>%
      # AVAL creation for severity records
      mutate(AVAL = case_when(
        AVALC == "NONE" ~ 0,
        AVALC == "MILD" ~ 1,
        AVALC == "MODERATE" ~ 2,
        AVALC == "SEVERE" ~ 3
      ))
    # events exclusions
    if (is.null(exclude_events)) {
      pp <- maxsev_pp
    } else {
      pp <- maxsev_pp %>% filter(!(FAOBJ %in% exclude_events))
    }
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
        "EXTRT",
        "AVAL",
        "AVALC",
        "FATEST",
        "FATESTCD",
        "DTYPE",
        "AVISIT",
        "AVISITN",
        "FASCAT",
        "FACAT"
      )

    # maximum severity derivation

    maxsev <- pp %>%
      group_by(!!!by_vars) %>%
      filter(!is.na(AVAL) & !(AVAL == "")) %>%
      filter(AVAL == max(AVAL)) %>%
      select(any_of(retain_vars)) %>%
      distinct() %>%
      mutate(
        DTYPE = "MAXIMUM",
        FATEST = test_maxsev,
        FATESTCD = testcd_maxsev
      )
    # binding with input data

    data.frame(bind_rows(maxsev_pp, maxsev))
  } else {
    print(paste0(filter_sev, " ", "doesn't exist in the FATESTCD"))
  }
}
