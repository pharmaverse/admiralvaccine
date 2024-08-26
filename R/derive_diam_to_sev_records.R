#' Creating Severity Records From Diameter
#'
#' @description
#' To derive the severity records from the diameter records.
#'
#' @param dataset Input data set
#'
#' The variables `USUBJID`,`FAOBJ`,`AVAL`, `AVALC`, `FATESTCD` and `FATEST` are expected
#' for Input data set.
#' @param filter_add filter for the `dataset`.
#' @param diam_code Diameter record filter
#'
#' *Permitted Value*: A character vector or scalar.
#'
#' Helps to filter the diameter records to derive the severity records by
#' passing the `FATESTCD` value for diameter which is corresponding to the
#' specified events in `faobj_values`.
#'
#' @param faobj_values Event filter
#'
#' *Permitted Value*: A character vector or Scalar.
#'
#'  Helps to filter the events (`Redness` and `Swelling`) which has diameter records
#'  to derive severity records by passing the events from `FAOBJ`.
#'
#' @param testcd_sev To assign `FATESTCD` value for severity
#'
#' *Permitted Value*: A character scalar
#'
#' Assign the value for `FATESTCD` variable to indicate the severity records.
#' Ignore the argument if you want to set the default value (`SEV`).
#'
#' @param test_sev `FATEST` Value for severity
#'
#' *Permitted Value*: A Character scalar
#'
#' Assign the value for `FATEST` variable to indicate the severity records.
#' Ignore the argument if you want to set the default value.
#'
#' @param none Pass the lower limit for grade `"NONE"`
#'
#' *Permitted Value:* A numeric vector
#'
#' The `none` and the following arguments (`mild`, `mode` and `sev`) will be
#' used for assigning the diameter limit to derive the `AVALC` (severity grade).
#'
#' *Assign the lower limit to derive the Severity Grade (`AVALC`).*\cr
#' *For Example: User passing 0 to `none` and 2 to `mild`, 0 will act as lower limit and 2 will act*
#' *as upper limit.*\cr
#'
#' *Note: Use the limit reference to pass the values to these arguments*\cr
#'  *Since the condition was coded like this,*\cr
#'  *NONE : `none` < AVAL <= `mild`*\cr
#'  *MILD : `mild` < AVAL <= `mod`*\cr
#'  *MODERATE : `mod` < AVAL <= `sev`*\cr
#'  *SEVERE : `sev` < AVAL*\cr
#'  *User should pass the values as numeric scalar. Refer the default values.*
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
#' @note Basically, This function will derive and create the severity records from the
#' diameter record for the particular events specified in the `faobj_values` that user wants.
#' If you want to derive the Severity from diameter, even though you have the severity in SDTM data.
#' This function will re-derive the severity and remove the derived SDTM severity records.
#'
#' @return The Input data with the new severity records for Redness and swelling which
#' is specified in `faobj_values` and AVAL, AVALC will be derived and `FATESTCD`,
#'  `FATEST` will be changed as per the values.
#'
#' @author Arjun Rubalingam
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
#' derive_diam_to_sev_records(
#'   dataset = input,
#'   faobj_values = c("REDNESS", "SWELLING"),
#'   diam_code = "DIAMETER",
#'   testcd_sev = "SEV",
#'   test_sev = "Severity"
#' )
#'
#' @keywords der_rec
#' @family der_rec
#'
derive_diam_to_sev_records <- function(dataset,
                                       filter_add = NULL,
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

  filter_add <- assert_filter_cond(enexpr(filter_add), optional = TRUE)

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
      if (!is.null(filter_add)) {
        fil_rec <- fil_rec %>%
          filter(!!filter_add)
      }
      sev <- fil_rec %>%
        filter(FAOBJ %in% faobj_values & FATESTCD %in% diam_code) %>%
        mutate(
          FATESTCD = testcd_sev,
          FATEST = test_sev,
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
