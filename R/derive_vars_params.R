#' `derive_vars_params.R`
#'
#' Creating `PARAMCD` from lookup file and assigning `PARAM`,`PARAMN`,`PARCAT1`,
#' `PARCAT2` variables
#'
#' @param dataset - Input dataset
#'        Input dataset is expected to have variables `USUBJID`,`FAOBJ`,
#'        `FACAT`, `FATESTCD` and `FATEST`
#'
#' @param lookup_dataset - lookup file containing `PARAMCD` values for every
#'        unique `FATESTCD` and `FAOBJ`
#'        lookup file is expected to have variables `FATEST`, `PARAMCD`,
#'        `FATESTCD`, `FAOBJ` and one entry for every unique
#'        `FATESTCD` and `FAOBJ`
#'
#' @return The output dataset contains all observations and variables of the
#'   input dataset along with `PARAM`,`PARAMCD`,`PARCAT1`,`PARCAT2`,`PARAMN`
#'
#' @author Dhivya Kanagaraj
#'
#' @details A lookup file is required with PARAMCD values for every combination
#'      of `FATEST` & `FAOBJ`.
#'      `PARAMCD` values comes from lookup file.
#'      `PARAMN` is assigned with a unique number for every unique PARAM value.
#'      `PARAM` value is a combination of `FAOBJ` `FATEST` `FASTRESU` `FALOC`
#'      `FADIR` `FALAT`
#'
#' @export
#'
#' @family der_adxx
#'
#' @keywords der_adxx
#'
#' @examples
#'
#' library(tibble)
#' lookup_dataset <- tribble(
#'   ~FATESTCD,    ~PARAMCD,    ~FATEST,                ~FAOBJ,
#'   "SEV",        "SEVREDN",   "Severity",             "Redness",
#'   "DIAMETER",   "DIARE",     "Diameter",             "Redness",
#'   "MAXDIAM",    "MDIRE",     "Maximum Diameter cm",  "Redness",
#'   "MAXTEMP",    "MAXTEMP",   "Maximum Temperature",  "Fever",
#'   "OCCUR",      "OCFEVER",   "Occurrence Indicator", "Fever",
#'   "OCCUR",      "OCERYTH",   "Occurrence Indicator", "Erythema",
#'   "SEV",        "SEVPAIN",   "Severity",             "Pain at Injection site",
#'   "OCCUR",      "OCPAIN",    "Occurrence Indicator", "Pain at Injection site",
#'   "OCCUR",      "OCSWEL",    "Occurrence Indicator", "Swelling"
#' )
#'
#' input <- tribble(
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
#' derive_vars_params(
#'   dataset = input,
#'   lookup_dataset = lookup_dataset
#' )
#'
derive_vars_params <- function(dataset,
                               lookup_dataset) {
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, FAOBJ)
  )
  # Merging lookup file to get PARAMCD values
  adface <- derive_vars_merged(
    dataset,
    dataset_add = lookup_dataset,
    new_vars = exprs(PARAMCD),
    by_vars = exprs(FATESTCD, FAOBJ)
  ) %>%
    convert_blanks_to_na()

  # Checking if permissible variable exisits in dataset
  lookup <-
    c("FASTRESU", "FALOC", "FADIR", "FALAT")

  # Assigning PARCAT1 PARCAT2 & PARAM
  adface <- adface %>%
    mutate(
      PARCAT1 = FACAT,
      PARCAT2 = FASCAT,
      PARAM = ""
    ) %>%
    unite(PARAM, FAOBJ, FATEST, any_of(lookup),
      sep = " ",
      na.rm = TRUE, remove = FALSE
    ) %>%
    mutate(PARAM = str_to_sentence(PARAM))

  # Assigning PARAMN
  paramn <- adface %>%
    distinct(PARAM, .keep_all = FALSE) %>%
    mutate(PARAMN = 1:n())

  adface <- merge(
    x = adface,
    y = paramn,
    by = c("PARAM"),
    all.x = TRUE
  )
  return(adface)
}

# ________________________END OF THE FUNCTION___________________________________
