#' Assigning Parameter Variables
#'
#' Creating `PARAMCD` from lookup dataset and assigning `PARAM`,`PARAMN`,`PARCAT1`,
#' `PARCAT2` variables
#'
#' @param dataset Input dataset
#'        Input dataset is expected to have variables `USUBJID`,`FAOBJ`,
#'        `FACAT`, `FATESTCD` and `FATEST`
#'
#' @param lookup_dataset lookup dataset containing `PARAMCD` values for every
#'        unique `FATESTCD` and `FAOBJ`
#'        lookup dataset is expected to have variables `FATEST`, `PARAMCD`,
#'        `FATESTCD`, `FAOBJ` and one entry for every unique
#'        `FATESTCD` and `FAOBJ`
#'
#' @param merge_vars List of Variables need to be merged from lookup dataset
#'
#' @return The output dataset contains all observations and variables of the
#'   input dataset along with `PARAM`,`PARAMCD`,`PARCAT1`,`PARCAT2`,`PARAMN`
#'
#' @author Dhivya Kanagaraj
#'
#' @details A lookup dataset is required with `PARAMCD` values for every combination
#'      of `FATEST` & `FAOBJ`.
#'      `PARAMCD` `PARAMN` `PARAMN` `PARCAT1` `PARCAT2` values can be assigned
#'      from lookup dataset.
#'
#'      if `PARAMN` not assigned in lookup dataset then
#'      `PARAMN` is assigned with a unique number for every unique PARAM value.
#'      if `PARAM` value not assigned in lookup dataset then
#'      `PARAM` value is a combination of `FAOBJ` `FATEST` `FASTRESU` `FALOC`
#'      `FADIR` `FALAT`
#'      if `PARCAT1` value not assigned in lookup dataset then
#'      `PARCAT1` is assigned as `FACAT`
#'      if `PARCAT2` value not assigned in lookup dataset then
#'      `PARCAT2` is assigned as `FASCAT`
#'
#' @export
#'
#' @family der_var
#'
#' @keywords der_var
#'
#' @examples
#'
#' library(admiral)
#' library(tibble)
#' library(dplyr)
#'
#' lookup_dataset <- tibble::tribble(
#'   ~FATESTCD, ~PARAMCD, ~PARAMN, ~FATEST, ~FAOBJ,
#'   "SEV", "SEVREDN", 1, "Severity", "Redness",
#'   "DIAMETER", "DIARE", 2, "Diameter", "Redness",
#'   "MAXDIAM", "MDIRE", 3, "Maximum Diameter cm", "Redness",
#'   "MAXTEMP", "MAXTEMP", 4, "Maximum Temperature", "Fever",
#'   "OCCUR", "OCFEVER", 5, "Occurrence Indicator", "Fever",
#'   "OCCUR", "OCERYTH", 6, "Occurrence Indicator", "Erythema",
#'   "SEV", "SEVPAIN", 7, "Severity", "Pain at Injection site",
#'   "OCCUR", "OCPAIN", 8, "Occurrence Indicator", "Pain at Injection site",
#'   "OCCUR", "OCSWEL", 9, "Occurrence Indicator", "Swelling"
#' )
#'
#' input <- tibble::tribble(
#'   ~USUBJID, ~FACAT, ~FASCAT, ~FATESTCD, ~FAOBJ, ~FATEST, ~FALOC, ~FALAT,
#'   "ABC101", "REACTO", "ADMIN", "SEV", "Redness", "Severity", "ARM", "LEFT",
#'   "ABC101", "REACTO", "ADMIN", "DIAMETER", "Redness", "Diameter", "ARM", "RIGHT",
#'   "ABC101", "REACTO", "ADMIN", "MAXDIAM", "Redness", "Maximum Diameter", NA, NA,
#'   "ABC101", "REACTO", "SYSTEMIC", "MAXTEMP", "Fever", "Maximum Temp", NA, NA,
#'   "ABC101", "REACTO", "SYSTEMIC", "OCCUR", "Fever", "Occurrence", NA, NA,
#'   "ABC101", "REACTO", "ADMIN", "OCCUR", "Erythema", "Occurrence", NA, NA,
#'   "ABC101", "REACTO", "ADMIN", "SEV", "Swelling", "Severity", NA, NA,
#'   "ABC101", "REACTO", "ADMIN", "OCCUR", "Swelling", "Occurrence", NA, NA,
#'   "ABC101", "REACTO", "ADMIN", "OCCUR", "Swelling", "Occurrence", NA, NA
#' )
#'
#' derive_vars_params(
#'   dataset = input,
#'   lookup_dataset = lookup_dataset,
#'   merge_vars = exprs(PARAMCD, PARAMN)
#' )
#'
derive_vars_params <- function(dataset,
                               lookup_dataset,
                               merge_vars) {
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, FAOBJ, FATEST, FATESTCD)
  )
  # Merging lookup dataset to get PARAMCD values
  adface <- derive_vars_merged(
    dataset,
    dataset_add = lookup_dataset,
    new_vars = merge_vars,
    by_vars = exprs(FATESTCD, FAOBJ)
  ) %>%
    convert_blanks_to_na()

  # Checking if permissible variable exists in dataset
  lookup <-
    c("FALOC", "FADIR", "FALAT")

  # Assigning PARCAT1 PARCAT2 & PARAM
  if (!("PARAM" %in% names(adface))) {
    adface <- adface %>%
      mutate(
        PARAM = ""
      ) %>%
      unite(PARAM, FAOBJ, FATEST, any_of(lookup),
        sep = " ",
        na.rm = TRUE, remove = FALSE
      ) %>%
      mutate(PARAM = str_to_sentence(PARAM))
  }

  # Assigning PARAMN
  if (!("PARAMN" %in% names(adface))) {
    paramn <- adface %>%
      distinct(PARAM, .keep_all = FALSE) %>%
      mutate(PARAMN = seq_len(n()))

    adface <- merge(
      x = adface,
      y = paramn,
      by = c("PARAM"),
      all.x = TRUE
    )
  }

  if (!("PARCAT1" %in% names(adface))) {
    adface <- adface %>%
      mutate(PARCAT1 = FACAT)
  }

  if (!("PARCAT2" %in% names(adface))) {
    adface <- adface %>%
      mutate(PARCAT2 = FASCAT)
  }
  return(adface)
}
