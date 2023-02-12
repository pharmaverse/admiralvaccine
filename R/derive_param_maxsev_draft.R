#' Derive Maximum Severity
#'
#' Derive the maximum severity for each administrative and Systemic events.                                      +
#'
#' @details
#'
#' Pre_processing:
#'
#' Severity records for Administrative and systemic events will be filtered
#' from the input data set and AVAL will be derived in a data_frame pp
#'
#' Maximum severity derivation:
#'
#' Once the pre-processing is done. The maximum severity records will be derived
#' from the  AVAL by USUBJID and by_vars.
#' Below variables will be created with the values mentioned below.
#' 'DTYPE = "MAXIMUM", FATESTCD = "MAXSEV" and FATEST = "MAXIMUM SEVERITY".'
#'
#' @param dataset
#' The Input data frame for this function
#' required variables for Input data freame :USUBJID, ATPTREF(FATPTREF),
#' PARCAT2(FASCAT), AVAL(FASTRESN),AVALC(FASTRESC),FAOBJ,FATESTCD,FATEST.
#'
#' @param testcd_sev
#' In testcd_sev, Pass the testcd in the quotes ("SEV") which is representing
#' the severity records.if you have more than one testcd, pass it as a list in
#' the testcd_sev arugument.
#'
#' @param by_vars
#' In by_vars, pass the inside vars() grouping variables to derive the Maximum severity
#' eg: by_vars= vars(USUBJID,FAOBJ, ATPTREF)
#'
#'
#' @return Dataframe which has Maxsev records (with DTYPE as "MAXIMUM", Derived AVAL
#' and AVALC and some grouping vars and updated FATESTCD and FATEST.)
#'
#' @keywords der_adxx
#' @family der_adxx
#'
#'
derive_param_maxsev <- function(
  dataset = NULL,
  testcd_sev = 'SEV',
  by_vars = NULL
){
  # assertions
  assert_data_frame(dataset,
                    required_vars = vars(USUBJID, PARCAT2, ATPTREF, AVAL, AVALC,
                                         FAOBJ, FATEST, FATESTCD))
  assert_character_vector(testcd_sev,optional = FALSE)
  assert_vars(by_vars)

  # filtering the severity record

  pp <- dataset %>%
    filter(FATESTCD %in% testcd_sev &
             PARCAT2  %in% c("ADMINISTRATION SITE", "SYSTEMIC")) %>%
    mutate(
   # AVAL derivation
      AVAL = case_when(
        AVALC == "NONE" ~ 0,
        AVALC == "MILD" ~ 1,
        AVALC == "MODERATE" ~ 2,
        AVALC == "SEVERE" ~ 3,
        AVALC == "GRADE4" ~ 4 )
    )

  # deriving the maximum severity

  maxsev <- pp %>%
    group_by(!!!by_vars) %>%
    summarise(AVAL = max(AVAL))%>%
    mutate(
      AVALC = case_when(
        AVAL == 0 ~ "NONE",
        AVAL == 1 ~ "MILD",
        AVAL == 2 ~ "MODERATE",
        AVAL == 3 ~ "SEVERE"
      ),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXSEV",
      FATEST = "Maximum Severity"
    )

  return(maxsev)
}

