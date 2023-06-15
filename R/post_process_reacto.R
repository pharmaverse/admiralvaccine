#' Post processing function for ADFACE dataset
#'
#' @description This is used to do post processing for ADaM reactogenicity dataset, where we derived
#' the SDTM level records, the corresponding values in FA variables will be `NA`.
#'
#' @param dataset Input dataset
#' @param filter_dataset filter condition
#' convert the FA variables records to NA depends on this condition.
#'
#' @return The input dataframe with `NA` values in FA variables where the SDTM records modified for
#' ADaM derivation purpose.
#' @export
#'
#' @examples
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~FALAT, ~FACAT, ~FASCAT, ~FATPT, ~FATESTCD, ~PARAMCD, ~AVAL,
#'   "ABC-1001", "FEVER", NA, "REACTO", "SYS", "DAY 1", "MAXTEMP", "MAXTEMP", 39.4,
#'   "ABC-1001", "VOMITING", NA, "REACTO", "SYS", "DAY 4", "MAXSEV", "MAXVOMIT", 3,
#'   "ABC-1001", "SWELLING", "LEFT", "REACTO", "ADMIN", "DAY 1", "MAXSEV", "MAXSWEL", 3,
#'   "ABC-1001", "REDNESS", "LEFT", "REACTO", "ADMIN", "DAY 2", "DIAMATER", "DIARE", 10.3,
#'   "ABC-1001", "FEVER", "LEFT", "REACTO", "SYS", "DAY 2", "OCCUR", "OCCFEV", NA
#' )
#'
#' post_process_reacto(
#'   dataset = input,
#'   filter_dataset = FATESTCD %in% c("MAXSEV", "MAXTEMP") |
#'     (FATESTCD == "OCCUR" & FAOBJ == "FEVER")
#' )
#'
post_process_reacto <- function(
    dataset,
    filter_dataset = FATESTCD %in% c("MAXDIAM", "MAXSEV", "MAXTEMP") |
      (FATESTCD == "OCCUR" & FAOBJ == "FEVER")) {
  assert_data_frame(dataset, required_vars = exprs(USUBJID, FATESTCD, FAOBJ))
  filter_dataset <- assert_filter_cond(enexpr(filter_dataset))

  # creating a vector of FA variables.
  favars <- names(dataset %>% select(starts_with("FA"), FATESTCD))

  dataset %>% mutate(across(
    favars,
    function(x) ifelse(!!filter_dataset, NA, x)
  ))
}

post_process_reacto(adface)