#' Creating Maximum flags
#'
#' Adds Flags variables for maximum record per subject per event for overall
#' and per vaccination
#'
#' @param dataset Input dataset
#'
#' @param flag1 - Flags the maximum record per subject per event per
#'                  Vaccination
#' *Default: "ANL01FL"*
#' *Permitted value: Any variable name or NULL*
#' `NULL` denotes not to create the flag
#'
#' @param flag2 - Flags the maximum record per subject per event for Overall
#' *Default: "ANL02FL"*
#' *Permitted value: Any variable name or NULL*
#' `NULL` denotes not to create the flag
#'
#' @return The output dataset creates `ANLxxFL` flags
#'
#' @author Dhivya Kanagaraj
#'
#' @details This utility flags the maximum record per subject per event
#'          per vaccination/Overall
#'          If both parameters `flag1` & `flag2` are passed as NULL then
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
    required_vars = exprs(USUBJID, FAOBJ)
  )
  if (is.null(flag1) && is.null(flag2)) {
    stop("Please mention flag name")
  }


  flag <- function(dataset,
                   by_vars,
                   fl) {
    temp <- dataset %>%
      filter(!is.na(AVAL)) %>%
      group_by(!!!by_vars) %>%
      arrange(desc(AVAL), FATPT, .by_group = TRUE) %>%
      filter(AVAL == max(AVAL)) %>%
      mutate(
        !!fl := ifelse(row_number() == 1 & AVAL > 0, "Y", NA_character_)
      )


    dataset <- left_join(
      x = dataset,
      y = temp,
      keep = FALSE
    )
  }


  if (!is.null(flag1) && !is.null(flag2)) {
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
      fl = flag1
    )
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, PARAMCD),
      fl = flag2
    )
  } else if (!is.null(flag1)) {
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
      fl = flag1
    )
  } else if (!is.null(flag2)) {
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, PARAMCD),
      fl = flag2
    )
  }
}
