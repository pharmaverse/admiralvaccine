#' derive_param_fever_occur.R
#'
#' Getting `FAOBJ = "FEVER"` records from `SDTM.VS` dataset, if there are no
#' FEVER records in input dataset
#'
#' @param dataset Input dataset
#'      Input dataset is expected to have variables `USUBJID`,`FAOBJ`,
#'      `FACAT`,`FATESTCD` and `FATEST`
#'
#' @param source_data Input SDTM dataset(VS)
#'       Input SDTM dataset is expected to have
#'       `VSCAT = "REACTOGENICITY" & VSTESTCD = "TEMP"`records
#' *Default: "VS"*
#'
#' @param faobj  FAOBJ Value for FEVER records in output dataset
#' *Default: "FEVER"*
#'
#' @return The output dataset contains records with `FATESTCD = "OCCUR"`for
#' `FAOBJ = FEVER` records.
#'
#' @author Dhivya Kanagaraj
#'
#' @details Check if `FAOBJ = FEVER` record is present in input dataset,
#'          if not then use `SDTM.VS` to get FEVER records.
#'          With temperature values from VSSTRESN we decide if FEVER
#'          has occurred or not(`FAORRES = "Y"/"N"`).
#'          Since records are derived, these FEVER records are considered
#'          `DTYPE = "DERIVED"`
#' @export
#'
#' @family der_adxx
#'
#' @keywords der_adxx
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(admiraldev)
#' library(admiral)
#'
#' input <- tribble(
#'   ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT,
#'   "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",
#'   "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",
#'   "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",
#'   "ABC101", "FATIQUE", "OCCUR", "REACTOGENICITY", "SYSTEMIC", "DAY 3"
#' )
#'
#' vs <- tribble(
#'   ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
#'   "ABC101", "TEMP", "REACTOGENICITY", 38.3, "C", "DAY 1",
#'   "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 2",
#'   "ABC101", "TEMP", "REACTOGENICITY", 36, "C", "DAY 3",
#'   "ABC101", "TEMP", "REACTOGENICITY", 37, "C", "DAY 4",
#'   "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 5",
#'   "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 6",
#'   "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 7"
#' )
#'
#' derive_param_fever_occur(
#'   dataset = input,
#'   source_data = vs,
#'   faobj = "FEVER"
#' )
#'
derive_param_fever_occur <- function(dataset,
                                     source_data,
                                     faobj) {
  # Checking if there are fever records in face
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, FAOBJ)
  )
  assert_data_frame(source_data,
    required_vars = exprs(USUBJID, VSTESTCD)
  )

  temp1 <- dataset %>% filter(FAOBJ == faobj)
  x <- nrow(temp1)
  # Renaming VS variables as FA variables
  lookup <-
    c(
      FASEQ = "VSSEQ",
      FACAT = "VSCAT",
      FAREASND = "VSREASND",
      FSSCAT = "VSSCAT",
      FAEVAL = "VSEVAL",
      FARFTDTC = "VSRFTDTC",
      FAEVLINT = "VSEVLINT",
      FAEVINTX = "VSEVINTX",
      FADTC = "VSDTC",
      FADY = "VSDY",
      FATPTREF = "VSTPTREF",
      FATPTNUM = "VSTPTNUM",
      FALNKID = "VSLNKID",
      FALNKGRP = "VSLNKGRP",
      FATPT = "VSTPT"
    )
  # Getting fever records from VS
  if (x == 0) {
    # Filter Reacto records from VS

    fever <- source_data %>%
      filter(VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP") %>%
      rename(any_of(lookup)) %>%
      mutate(
        FATEST = "Occurrence Indicator",
        FATESTCD = "OCCUR",
        FASCAT = "SYSTEMIC",
        FAOBJ = faobj
      ) %>%
      # Creating occurance records for fever
      mutate(
        FAORRES = case_when(
          VSSTRESU == "C" &
            VSSTRESN >= 38 ~ "Y",
          VSSTRESU == "C" &
            VSSTRESN < 38 ~ "N"
        ),
        FASTRESC = FAORRES,
        DTYPE = "DERIVED"
      )
    adface <- bind_rows(dataset, fever)
    return(adface)
  }
}

# ________________________END OF THE FUNCTION___________________________________
