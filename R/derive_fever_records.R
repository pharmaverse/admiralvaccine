#' Creating Fever Records
#'
#' Creating Fever records from the `VS` SDTM dataset.
#'
#' @param dataset Input Dataset
#' Input dataset is expected to have variables `USUBJID` and `FAOBJ`.
#'
#' @param dataset_source Source Dataset - SDTM Vital Sign (`VS`)
#' Source Dataset (VS) is expected to have temperature records.
#'
#' @param filter_source Filter condition for Source dataset.
#'
#' @param faobj FAOBJ Value for fever records in output dataset.
#'
#' @return The output dataset contains records with `FATESTCD = "OCCUR"` for
#' `FAOBJ = FEVER` records.
#'
#' @export
#'
#' @details Check if `FAOBJ = FEVER` record is present in input dataset,
#' if not then use `SDTM.VS` to get FEVER records.
#' With temperature values from `VSSTRESN` we decide if FEVER has
#' occurred or not (`FAORRES = "Y"/"N"`).
#' Since records are derived, these FEVER records are considered `DTYPE = "DERIVED"`
#' if `FAOBJ = FEVER` record is present, then input dataset will be made as output with no further
#' analysis.
#'
#' The temperature value greater or equal 38° C will be considered as FEVER records.
#'
#' @author Dhivya Kanagaraj
#'
#' @family der_rec
#'
#' @keywords der_rec
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
#' derive_fever_records(
#'   dataset = input,
#'   dataset_source = vs,
#'   filter_source = VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP",
#'   faobj = "FEVER"
#' )
#'
derive_fever_records <- function(dataset,
                                 dataset_source,
                                 filter_source,
                                 faobj) {
  # Checking if there are fever records in face
  assert_data_frame(dataset,
    required_vars = exprs(USUBJID, FAOBJ)
  )
  assert_data_frame(dataset_source,
    required_vars = exprs(USUBJID, VSTESTCD, VSSTRESN, VSSTRESU)
  )
  filter_source <- assert_filter_cond(enexpr(filter_source))

  # filtering whether we have fever records in input dataset
  fil_fev <- dataset %>% filter(FAOBJ == faobj)
  row_rec <- nrow(fil_fev)
  # Renaming VS variables as FA variables
  vs_to_fa <-
    c(
      FASEQ = "VSSEQ",
      FAREASND = "VSREASND",
      FASCAT = "VSSCAT",
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
  if (row_rec == 0) {
    # Filter Reacto records from VS

    fev_rec <- dataset_source %>%
      filter(!!filter_source) %>%
      rename(any_of(vs_to_fa)) %>%
      mutate(
        FATEST = "Occurrence Indicator",
        FATESTCD = "OCCUR",
        FACAT = "REACTOGENICITY",
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
        FASTRESC = FAORRES
      ) %>%
      select(-(starts_with("VS")), VSSTRESN) # nolint

    bind_rows(dataset, fev_rec)
  } else if (row_rec > 0) {
    return(dataset)
  }
}
