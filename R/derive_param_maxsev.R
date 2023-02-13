#' Creating Maximum Severity Records
#'
#' Creating the maximum severity records per subject per event per period.
#' Maximum severity records will be derived for the Administration and systemic
#' events which has severity records `FATESTCD = "SEV"`.
#'
#'
#' @param dataset input data set
#'
#'  The variables `USUBJID`, `APTPTREF`, `FAOBJ`, `FASCAT`, `AVALC`, `FAOBJ`,
#'  `FATESTCD` and `FATEST` are expected for input data set.(`dataset`)
#'
#' @param red_swell flag to include redness and swelling records
#'
#' *Default: "N"*
#' *Permitted Value*: A character scalar
#'
#' Set to "Y" will include the severity observation where`FATESTCD = SEV` for
#' redness and swelling to derive the maximum severity records.
#' To exclude the redness and swelling, set the `red_swell` to `"N"` which is
#' default.
#'
#' *Note:* Prior to Set the flag `red_swell` to `"Y"`, we should have create
#' the severity records for Redness and Swelling by using the function called
#' `derive_param_diam_to_sev.R`.
#'
#' @param filter_sev severity record filter
#'
#' *Default: "SEV"*
#' *Permitted Value*: A character scalar
#'
#' Helps to filter the severity records to derive the maximum severity by passing
#' the `FATESTCD` value for severity in quotes `"SEV"` for this argument.
#'
#' @param test_maxsev fatest value for maximum severity records
#'
#' *Default: "Maximum severity"*
#' *Permitted Value*: A character scalar
#'
#' Assign the value for `FATEST` variable to indicate the maximum severity records.
#' Ignore the argument if you want to set the value as mentioned in default.
#'
#' @param testcd_maxsev fatestcd value for maximum severity records
#'
#' *Default: "MAXSEV"*
#' *Permitted Value*: A character scalar
#'
#' Assign the value for `FATESTCD` variable to indicate the maximum severity
#' records. Ignore the argument if you want to set the value as mentioned in default.
#'
#' @param by_vars grouping variables
#'
#' *Default: vars(USUBJID, FAOBJ, ATPTREF)*
#'
#' Based on the variables which will be passed on this `by_vars` argument, The
#' maximum severity records will be derived from AVAL.
#' *Note:* Pass the variables in `vars()`.
#'
#' @return
#' The input data set with new records `FATESTCD = MAXSEV`indicating the maximum
#' severity records for the specified variables in `by_vars`. `DTYPE` will be
#' populated as `MAXIMUM`. `FATEST`will be populated as specified in `test_maxsev`
#' and `FATESCD` will be populated as specified in `testcd_maxsev`.
#'
#' @author Arjun Rubalingam
#'
#' @export
#'
#'
#' @examples
#' input <- tribble(
#'   ~USUBJID,     ~FAOBJ,         ~AVAL,  ~AVALC,    ~ATPTREF,
#'   "XYZ1001",    "REDNESS",           2,   "MODERATE","VACCINATION 1",
#'   "XYZ1001",    "REDNESS",           1,   "MILD",    "VACCINATION 1",
#'   "XYZ1001",    "REDNESS",           0,   "NONE",    "VACCINATION 1",
#'   "XYZ1001",    "REDNESS",           0,   "NONE",    "VACCINATION 1",
#'   "XYZ1001",    "REDNESS",           0,   "NONE",    "VACCINATION 1",
#'   "XYZ1002",    "REDNESS",           3,   "SEVERE",  "VACCINATION 2",
#'   "XYZ1002",    "REDNESS",           2,   "MODERATE","VACCINATION 2",
#'   "XYZ1002",    "REDNESS",           2,   "MODERATE","VACCINATION 2",
#'   "XYZ1002",    "REDNESS",           1,   "MILD",    "VACCINATION 2",
#'   "XYZ1002",    "REDNESS",           0,   "NONE",    "VACCINATION 2",
#'   "XYZ1001",    "SWELLING",          2,   "MODERATE","VACCINATION 1",
#'   "XYZ1001",    "SWELLING",          1,   "MILD",    "VACCINATION 1",
#'   "XYZ1001",    "SWELLING",          0,   "NONE",    "VACCINATION 1",
#'   "XYZ1001",    "SWELLING",          0,   "NONE",    "VACCINATION 1",
#'   "XYZ1001",    "SWELLING",          0,   "NONE",    "VACCINATION 1",
#'   "XYZ1002",    "SWELLING",          3,   "SEVERE",  "VACCINATION 2",
#'   "XYZ1002",    "SWELLING",          2,   "MODERATE","VACCINATION 2",
#'   "XYZ1002",    "SWELLING",          1,   "MILD",    "VACCINATION 2",
#'   "XYZ1002",    "SWELLING",          0,   "NONE",    "VACCINATION 2",
#'   "XYZ1002",    "SWELLING",          0,   "NONE",    "VACCINATION 2",
#'   "XYZ1001",    "HEADACHE",          3,   "SEVERE",  "VACCINATION 1",
#'   "XYZ1001",    "HEADACHE",          1,   "MILD",    "VACCINATION 1",
#'   "XYZ1001",    "HEADACHE",          1,   "MILD",    "VACCINATION 1",
#'   "XYZ1001",    "HEADACHE",          0,   "NONE",    "VACCINATION 1",
#'   "XYZ1001",    "HEADACHE",          0,   "NONE",    "VACCINATION 1",
#'   "XYZ1002",    "HEADACHE",          2,   "MODERATE","VACCINATION 2",
#'   "XYZ1002",    "HEADACHE",          3,   "SEVERE",  "VACCINATION 2",
#'   "XYZ1002",    "HEADACHE",          3,   "SEVERE",  "VACCINATION 2",
#'   "XYZ1002",    "HEADACHE",          1,   "MILD",    "VACCINATION 2",
#'   "XYZ1002",    "HEADACHE",          1,   "MILD",    "VACCINATION 2"
#'
#' ) %>% mutate(
#'   AVALC = as.character(AVALC),
#'   FASCAT = case_when(
#'     FAOBJ == "HEADACHE" ~ "SYSTEMIC",
#'     TRUE ~ "ADMINISTRATION SITE"
#'   ),
#'   FATEST = "Severity",
#'   FATESTCD = "SEV"
#' )
#'
#' output<-derive_param_maxsev(
#'   dataset = input,
#'   red_swell = "Y",
#'   filter_sev = "SEV",
#'   test_maxsev = "Maximum Severity",
#'   testcd_maxsev = "MAXSEV"
#' )
#'
#' Creating the function
derive_param_maxsev <- function(dataset = NULL,
                                red_swell = "N",
                                filter_sev = "SEV",
                                test_maxsev = "Maximum Severity",
                                testcd_maxsev = "MAXSEV",
                                by_vars = vars(USUBJID, FAOBJ, ATPTREF)) {
  # assertions
  assert_data_frame(dataset,
                    required_vars = vars(USUBJID, FASCAT, AVALC, FAOBJ,
                                         ATPTREF, FATEST, FATESTCD))
  assert_character_scalar(filter_sev, optional = FALSE)
  assert_character_scalar(red_swell, optional = FALSE)
  assert_character_scalar(test_maxsev, optional = FALSE)
  assert_character_scalar(testcd_maxsev, optional = FALSE)
  assert_vars(by_vars, optional = FALSE)
  
  # pre-processing
  
  if (red_swell == "N") {
    maxsev_pp <- dataset %>%
      filter(
        FATESTCD == filter_sev &
          FASCAT %in% c('ADMINISTRATION SITE', 'SYSTEMIC') &
          !(FAOBJ %in% c("REDNESS", "SWELLING"))
      )
    
  } else if (red_swell == "Y") {
    maxsev_pp <- dataset %>%
      filter(FATESTCD == filter_sev &
               FASCAT %in% c('ADMINISTRATION SITE', 'SYSTEMIC'))
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
      "EXSTDTC",
      "EXENDTC",
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
  
  maxsev <- maxsev_pp %>%
    mutate(AVAL = case_when(
      AVALC == "NONE" ~ 0,
      AVALC == "MILD" ~ 1,
      AVALC == "MODERATE" ~ 2,
      AVALC == "SEVERE" ~ 3
    )) %>%
    group_by(!!!by_vars) %>%
    filter(AVAL == max(AVAL)) %>%
    select(any_of(retain_vars)) %>%
    distinct() %>%
    mutate(DTYPE = "MAXIMUM",
           FATEST = test_maxsev,
           FATESTCD = testcd_maxsev)
  # binding with input data
  
  maxsev_final <- bind_rows(dataset, maxsev)
  
  return(as.data.frame(maxsev_final))
  
}

