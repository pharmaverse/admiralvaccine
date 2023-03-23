#' `derive_vars_max_flag.R`
#'
#' @param `dataset` Input dataset
#'
#' @param `flag1` - Flags the maximum record per subject per event per
#'                  Vaccination
#' *Default: "ANL01FL"*
#' *Permitted value: Any variable name or NULL*
#' `NULL` denotes not to create the flag
#'
#' @param `flag2` - Flags the maximum record per subject per event for Overall
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
#' @family der_adxx
#'
#' @keywords der_adxx
#'
#' @examples
#' library(dplyr)
#' library(tidyverse)
#' library(admiraldev)
#' library(admiral)
#' library(tibble)
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~FATESTCD, ~FATPTREF, ~AVAL, ~FADTC, ~PARAMCD,
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 10, "2015-01-10", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 1", 7, "2015-01-11", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 3, "2015-02-10", "DIARE",
#'   "ABC101", "REDNESS", "DIAMETER", "VACC 2", 8, "2015-02-11", "DIARE",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-10", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 1", 1, "2015-01-11", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 2, "2015-02-10", "SEVFAT",
#'   "ABC101", "FATIQUE", "SEV", "VACC 2", 3, "2015-02-11", "SEVFAT"
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
  if (is.null(flag1) & is.null(flag2)) {
    stop("Please mention flag name")
  }

  flag <- function(dataset,
                   by_vars,
                   by_join,
                   fl) {
    temp <- dataset %>%
      filter(!is.na(AVAL)) %>%
      group_by(!!!by_vars) %>%
      arrange(desc(AVAL), FADTC, .by_group = TRUE) %>%
      summarise(FADTC = first(FADTC)) %>%
      mutate(!!fl := "Y") %>%
      ungroup()

    dataset <- left_join(
      x = dataset,
      y = temp,
      by = by_join,
      keep = FALSE
    )
  }

  if (!is.null(flag1) & !is.null(flag2)) {
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
      by_join = c("USUBJID", "FAOBJ", "FATPTREF", "PARAMCD", "FADTC"),
      fl = flag1
    )
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, PARAMCD),
      by_join = c("USUBJID", "FAOBJ", "PARAMCD", "FADTC"),
      fl = flag2
    )
  } else if (!is.null(flag1)) {
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, FATPTREF, PARAMCD),
      by_join = c("USUBJID", "FAOBJ", "FATPTREF", "PARAMCD", "FADTC"),
      fl = flag1
    )
  } else
  # Flagging maximum record per subject per event for Overall
  if (!is.null(flag2)) {
    dataset <- flag(dataset,
      by_vars = exprs(USUBJID, FAOBJ, PARAMCD),
      by_join = c("USUBJID", "FAOBJ", "PARAMCD", "FADTC"),
      fl = flag2
    )
  }
}

# ________________________END OF THE FUNCTION___________________________________
