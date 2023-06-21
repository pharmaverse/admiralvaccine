#' Creating Maximum Flag
#'
#' @description To Flag the maximum records depends on the grouping variables in a flag variable.
#'
#' @param dataset Input dataset
#'
#' @param by_vars By variables which goes to group by, to create the flag. Pass the variables
#' inside the exprs().
#'
#' @param fl Flag variable name, Pass it as string.
#'
#' @return Data frame with flag variable which is flagged for the maximum value records depending on
#' the variables passed in `by_vars` by user.
#'
#' @author Dhivya Kanagaraj
#' @export
#'
#' @examples
#'
#' library(tibble)
#' library(admiral)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT"
#' )
#'
#' max_flag(
#'   dataset = input,
#'   by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
#'   fl = "ANL01FL"
#' )
#'
#' @family utils_help
#'
#' @keywords utils_help
#'
max_flag <- function(dataset,
                     by_vars,
                     fl) {
  assert_vars(by_vars)
  assert_data_frame(dataset,
    required_vars = exprs(AVAL, FATPT)
  )

  temp <- dataset %>%
    filter(!is.na(AVAL)) %>%
    group_by(!!!by_vars) %>%
    arrange(desc(AVAL), FATPT, .by_group = TRUE) %>%
    filter(AVAL == max(AVAL)) %>%
    mutate(
      !!fl := ifelse(row_number() == 1 & AVAL > 0, "Y", NA_character_)
    )

  left_join(
    x = dataset,
    y = temp,
    keep = FALSE
  )
}

#' Creating ANLxxFL Variables To Flag The Maximum Records
#'
#' Adds Flags variables for maximum record per subject per event for overall
#' and per vaccination
#'
#' @param dataset Input dataset
#'
#' @param flag1 Flags the maximum record per subject per event per
#' vaccination.
#' *Permitted value: Any variable name as a string or NULL*.
#'
#' `NULL` denotes not to create the flag
#'
#' @param flag2 Flags the maximum record per subject per event for Overall
#'
#' *Permitted value: Any variable name as a string or NULL*.
#'
#' `NULL` denotes not to create the flag
#'
#' @return The output dataframe with `ANLxxFL` flags
#'
#' @author Dhivya Kanagaraj
#'
#' @details This utility flags the maximum record per subject per event
#'          per vaccination/Overall
#'          If both parameters `flag1` & `flag2` are passed as `NULL` then
#'          utility will throw error and flags will not be created.
#'
#' @export
#'
#' @family der_var
#'
#' @keywords der_var
#'
#' @examples
#' library(dplyr)
#' library(admiraldev)
#' library(admiral)
#' library(tibble)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FATPT, ~PARAMCD,
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "DAY 1", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "DAY 2", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "DAY 1", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "DAY 2", "DIARE",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 1", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "DAY 2", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "DAY 1", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "DAY 2", "SEVFAT"
#' )
#'
#' derive_vars_max_flag(
#'   dataset = input,
#'   flag1 = "ANL01FL",
#'   flag2 = "ANL02FL"
#' )
#' derive_vars_max_flag(
#'   dataset = input,
#'   flag1 = NULL,
#'   flag2 = "ANL02FL"
#' )
#' derive_vars_max_flag(
#'   dataset = input,
#'   flag1 = "ANL01FL",
#'   flag2 = NULL
#' )
#'
derive_vars_max_flag <- function(dataset,
                                 flag1 = "ANL01FL",
                                 flag2 = "ANL02FL") {
  # Flagging maximum record per subject per event per Vaccination
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD)
  )

  if (is.null(flag1) && is.null(flag2)) {
    stop("Both flag names cannot be NULL")
  }

  if (!is.null(flag1)) {
    dataset <- max_flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
      fl = flag1
    )
  }

  if (!is.null(flag2)) {
    dataset <- max_flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, PARAMCD),
      fl = flag2
    )
  }

  return(dataset)
}
