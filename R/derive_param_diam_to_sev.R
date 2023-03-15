#' Creating Severity Records from Diameters
#'
#' @description
#' To derive the severity records from the diameter records per subject per
#' event per period.
#'
#' @param dataset Input data set
#'
#' The variables `USUBJID`, `APTPTREF`, `FAOBJ`, `FASCAT`,`AVAL`, `AVALC`,
#' `FAOBJ`, `FATESTCD` and `FATEST` are expected for Input data set.(`data set`)
#'
#' @param filter_faobj Event filter
#'
#' *Default: c("REDNESS","SWELLING")*
#' *Permitted Value*: A character vector or Scalar.
#'
#'  Helps to filter the events (Redness and swelling) which has diameter records
#'  to derive severity records by passing the events from `FAOBJ`.
#'
#' @param filter_diam Diameter record filter
#'
#' *Default: "DIAMETER"*
#' *Permitted Value*: A character scalar.
#'
#' Helps to filter the diameter records to derive the severity records by
#' passing the `FATESTCD` value for diameter  which is corresponding to the
#' specified events in `filter_faobj`.
#'
#' @param testcd_sev `FATESTCD`value for severity
#'
#' *Default: "SEV"*
#' *Permitted Value*: A Character scalar
#'
#' Assign the value for `FATESTCD` variable to indicate the severity records.
#' Ignore the argument if you want to set the default value.
#'
#' *Note*: This argument value will be used to check whether input data set has
#' Severity records for specified `filter_faobj` event. If it has, those records
#' will be removed and new severity records will be derived from diameters.
#'
#' @param test_sev `FATEST` value for severity
#'
#' *Default: "Severity"*
#' *Permitted Value*: A Character scalar
#'
#' Assign the value for `FATEST` variable to indicate the severity records.
#' Ignore the argument if you want to set the default value.
#'
#' *Assign the Severity limits to create Severity grade(AVALC)*
#' @param none limit for grade `"NONE"`
#'
#' *Default: c(0,2)*
#' *Permitted value*: A Numeric vector
#'
#' The `none` and the following arguments(`mild`, `mode` and `sev` ) will be
#' used for assigning the diameter limits to derive the `AVALC`(severity grade).
#'
#'  *Note: Use the limit reference to pass the values in argument*
#'  *Since the condition was coded like this,*
#'  *NONE : none < AVAL <= none*
#'  *MILD : mild < AVAL <= mild*
#'  *MODERATE: mod < AVAL <= mod*
#'  *SEVERE: sev > AVAL*
#'  *User should pass the values as numeric vectors. Refer the default values.*
#'
#'
#' @param mild limit for grade `"MILD"`
#'
#'  *Default: c(2,5)*
#'  *Permitted value*: A Numeric vector
#'
#'
#' @param mod limit for grade `"MODERATE"`
#'
#' *Default: c(5,10)*
#' *Permitted value*: A Numeric vector
#'
#' @param sev limit for grade `"SEVERE"`
#'
#' *Default: 10*
#' *Permitted value*: A Numeric vector
#'
#' @details
#' 1. Pass the Input data set in `dataset` with required variables and `AVAL`
#' should have the diameter values for the events(`FAOBJ`) specified in
#' `filter_faobj`.
#' 2. If the SDTM data set has severity record for redness and swelling, User
#' can skip this function to keep the SDTM level severity records for further
#'  derivation.
#' 3. If User want to derive the severity records from the diameter records even
#'  that data set has severity records for Redness and swelling. This function
#'  will remove the existing severity records and it will create the new
#'  severity records as`AVALC` which is derived from diameter. and `AVAL` will
#'  be derived as numeric severity grade to get the maximum severity records
#'  which will be derived by `derive_param_maxsev.R`.
#' 4. Pass the values in severity grade arguments(`none`, `mild`, `mod`, `sev`)
#'  as per the study needs.
#' 5.User can pass the events which has diameter values to derive their severity
#'  records by passing the appropriate `filter_diam`, `filter_faobj`and severity
#'  grade limits.
#'
#' @return
#' The Input data with the new severity records for Redness and swelling which
#' is specified in `filter_faobj` and AVAL, AVALC will be derived and fatestcd,
#'  fatest will be changed as per the values
#'
#' @examples
#' library(dplyr)
#' library(admiral)
#' library(tibble)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~AVAL, ~AVALC, ~ATPTREF, ~FATEST, ~FATESTCD,
#'   "XYZ1001", "REDNESS", 7.5, "7.5", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "REDNESS", 3.5, "3.5", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "REDNESS", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "REDNESS", 1.8, "1.8", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "REDNESS", 1.4, "1.4", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1002", "REDNESS", 11.1, "11.1", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "REDNESS", 7.4, "7.4", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "REDNESS", 6, "6", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "REDNESS", 2.1, "2.1", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "REDNESS", 1.1, "1.1", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1001", "SWELLING", 5.5, "5.5", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "SWELLING", 2.5, "2.5", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "SWELLING", 2, "2", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "SWELLING", 1.8, "1.8", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1001", "SWELLING", 1.4, "1.4", "VACCINATION 1", "Diameter", "DIAMETER",
#'   "XYZ1002", "SWELLING", 10.1, "10.1", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "SWELLING", 7.1, "7.1", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "SWELLING", 5, "5", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "SWELLING", 1.8, "1.8", "VACCINATION 2", "Diameter", "DIAMETER",
#'   "XYZ1002", "SWELLING", 1.4, "1.4", "VACCINATION 2", "Diameter", "DIAMETER"
#' )
#'
#' derive_param_diam_to_sev(
#'   dataset = input,
#'   filter_faobj = c("REDENSS", "SWELLING"),
#'   filter_diam = "DIAMETER",
#'   testcd_sev = "SEV",
#'   test_sev = "Severity"
#' )
#'
#' @export
#' @keywords der_adxx
#' @family der_adxx
#'
derive_param_diam_to_sev <- function(dataset = NULL,
                                     filter_diam = "DIAMETER",
                                     filter_faobj = c("REDNESS", "SWELLING"),
                                     testcd_sev = "SEV",
                                     test_sev = "Severity/Intensity",
                                     none = c(0, 2),
                                     mild = c(2, 5),
                                     mod = c(5, 10),
                                     sev = 10) {
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, AVAL, AVALC, FAOBJ, FATEST, FATESTCD)
  )
  assert_numeric_vector(arg = c(none, mild, mod, sev), optional = FALSE)
  assert_character_vector(
    arg = c(filter_diam, filter_faobj, testcd_sev, test_sev),
    optional = FALSE
  )

  # Checking & Removing the records which has severity records for the FAOBJ
  diam <- dataset %>% filter(FAOBJ %in% filter_faobj)
  if (testcd_sev %in% diam$FATESTCD) {
    fil_rec <- diam %>% filter(FATESTCD != testcd_sev)
  } else {
    fil_rec <- diam
  }

  # check if AVALC is a character vector
  if (class(fil_rec$AVALC) != "character") {
    stop(
      paste0("AVALC must be a character vector")
    )
  }

  # Replacing FATESTCD and FATEST for Diameter with Severity
  if (filter_diam %in% diam$FATESTCD) {
    sev <- fil_rec %>%
      mutate(
        FATESTCD = testcd_sev,
        FATEST = test_sev,
        DTYPE = "DERIVED",
        AVALC = if_else(
          none[1] <= AVAL & AVAL <= none[2],
          "NONE",
          if_else(
            mild[1] < AVAL & AVAL <= mild[2],
            "MILD",
            if_else(
              mod[1] < AVAL & AVAL <= mod[2],
              "MODERATE",
              if_else(sev[1] < AVAL, "SEVERE", AVALC)
            )
          )
        ),
        # Deriving AVAL
        AVAL = case_when(
          AVALC == "NONE" ~ 0,
          AVALC == "MILD" ~ 1,
          AVALC == "MODERATE" ~ 2,
          AVALC == "SEVERE" ~ 3
        ),
        AVAL = as.numeric(AVAL)
      )
    # binding with Input data set
    finalsev <- bind_rows(sev, fil_rec)
    return(data.frame(finalsev))
  } else {
    stop(
      paste0(filter_diam, " ", "doesn't exist in the filtered record")
    )
  }
}
