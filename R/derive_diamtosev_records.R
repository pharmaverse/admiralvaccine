#' Creating Severity Records From Diameters
#'
#' @description
#' To derive the severity records from the diameter records per subject per event per period.
#'
#' @param dataset Input data set
#'
#' The variables `USUBJID`, `APTPTREF`, `FAOBJ`, `FASCAT`,`AVAL`, `AVALC`,
#' `FAOBJ`, `FATESTCD` and `FATEST` are expected for Input data set.(`dataset`)
#'
#' @param diam_code Diameter record filter
#'
#' *Permitted Value*: A character vector or scalar.
#'
#' Helps to filter the diameter records to derive the severity records by
#' passing the `FATESTCD` value for diameter  which is corresponding to the
#' specified events in `faobj_values`.
#'
#' @param faobj_values Event filter
#'
#' *Permitted Value*: A character vector or Scalar.
#'
#'  Helps to filter the events (Redness and swelling) which has diameter records
#'  to derive severity records by passing the events from `FAOBJ`.
#'
#' @param testcd_sev `FATESTCD`value for severity
#'
#' *Permitted Value*: A Character scalar
#'
#' Assign the value for `FATESTCD` variable to indicate the severity records.
#' Ignore the argument if you want to set the default value.
#'
#' *Note*: This argument value will be used to check whether input data set has
#' Severity records for specified `faobj_values` event. If it has, those records
#' will be removed and new severity records will be derived from diameters.
#'
#' @param test_sev `FATEST` value for severity
#'
#' *Permitted Value*: A Character scalar
#'
#' Assign the value for `FATEST` variable to indicate the severity records.
#' Ignore the argument if you want to set the default value.
#'
#' *Assign the lower limit to derive the Severity Grade (AVALC)*
#' *For Example: User passing 0 to `none` and 2 to `mild`, 0 will act as lower limit and 2 will act*
#' *as upper limit*
#' *Note: Use the limit reference to pass the values in argument*
#'  *Since the condition was coded like this,*
#'  *NONE : none < AVAL <= mild*
#'  *MILD : mild < AVAL <= mod*
#'  *MODERATE: mod < AVAL <= sev*
#'  *SEVERE: sev < AVAL*
#'  *User should pass the values as numeric vectors. Refer the default values.*
#'
#' @param none Pass the lower limit for grade `"NONE"`
#'
#' *Permitted Value:* A numeric vector
#'
#' The `none` and the following arguments(`mild`, `mode` and `sev` ) will be
#' used for assigning the diameter limits to derive the `AVALC`(severity grade).
#'
#' @param mild Pass the lower limit for grade `"MILD"`
#'
#' *Permitted Value:* A numeric vector
#'
#' @param mod Pass the lower limit for grade `"MODERATE"`
#'
#' *Permitted Value:* A numeric vector
#'
#' @param sev Pass the lower limit for grade `"SEVERE"`
#'
#' *Permitted Value:* A numeric vector
#'
#' @return The Input data with the new severity records for Redness and swelling which
#' is specified in `faobj_values` and AVAL, AVALC will be derived and fatestcd,
#'  fatest will be changed as per the values.
#'
#' @export
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
#' derive_diamtosev_records(
#'   dataset = input,
#'   faobj_values = c("REDENSS", "SWELLING"),
#'   diam_code = "DIAMETER",
#'   testcd_sev = "SEV",
#'   test_sev = "Severity"
#' )
#'
#' @keywords der_rec
#' @family der_rec
#'
derive_diamtosev_records <- function(dataset = NULL,
                                     diam_code = "DIAMETER",
                                     faobj_values = c("REDNESS", "SWELLING"),
                                     testcd_sev = "SEV",
                                     test_sev = "Severity/Intensity",
                                     none = 0,
                                     mild = 2,
                                     mod = 5,
                                     sev = 10) {
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, AVAL, AVALC, FAOBJ, FATEST, FATESTCD)
  )

  assert_numeric_vector(arg = c(none, mild, mod, sev), optional = FALSE)

  assert_character_vector(
    arg = c(diam_code, faobj_values, testcd_sev, test_sev),
    optional = FALSE
  )

  # Checking & Removing the records which has severity records for the FAOBJ

  diam <- dataset %>% filter(FAOBJ %in% faobj_values)
  if (testcd_sev %in% diam$FATESTCD) {
    fil_rec <- dataset %>% filter(!(FAOBJ %in% faobj_values & FATESTCD == testcd_sev))
  } else {
    fil_rec <- dataset
  }
  # Replacing FATESTCD and FATEST for Diameter with Severity
  ds <- function(diam_code) {
    if (c(diam_code) %in% diam$FATESTCD) {
      sev <- fil_rec %>%
        filter(FAOBJ %in% faobj_values & FATESTCD %in% diam_code) %>%
        mutate(
          FATESTCD = testcd_sev,
          FATEST = test_sev,
          DTYPE = "DERIVED",
          FASEQ = NA_integer_,
          AVALC = if_else(
            none <= AVAL & AVAL <= mild,
            "NONE",
            if_else(
              mild < AVAL & AVAL <= mod,
              "MILD",
              if_else(
                mod < AVAL & AVAL <= sev,
                "MODERATE",
                if_else(sev < AVAL, "SEVERE", AVALC)
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

      return(sev)

    } else {
      warning(diam_code, " ", "doesn't exist in the filtered record")

      return(NULL)
    }
  }

  final <- lapply(diam_code, ds)
  do.call("bind_rows", final) %>%
    bind_rows(fil_rec)
}
