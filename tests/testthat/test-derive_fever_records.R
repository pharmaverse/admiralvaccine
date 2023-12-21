## Test 1: How the actual dataset is generated if FAOBJ="FEVER", if the FEVER records
## are not in FACE

test_that('derive_fever_records Test 1: How the actual dataset is generated
                    if FAOBJ="FEVER", if the FEVER records are not in FACE', {
  face <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT,
    "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",
    "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",
    "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1",
    "ABC101", "FATIQUE", "OCCUR", "REACTOGENICITY", "SYSTEMIC", "DAY 3"
  )

  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
    "ABC101", "TEMP", "REACTOGENICITY", 38.3, "C", "DAY 1",
    "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 2",
    "ABC101", "TEMP", "REACTOGENICITY", 36, "C", "DAY 3",
    "ABC101", "TEMP", "REACTOGENICITY", 37, "C", "DAY 4",
    "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 5",
    "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 6",
    "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 7"
  )

  expected1 <- vs %>%
    mutate(
      FAOBJ = "FEVER", FATESTCD = "OCCUR", FACAT = "REACTOGENICITY",
      FASCAT = "SYSTEMIC", FATEST = "Occurrence Indicator",
      FAORRES = ifelse(VSSTRESN >= 38, "Y", "N"),
      FASTRESC = ifelse(VSSTRESN >= 38, "Y", "N")
    ) %>%
    rename(FATPT = VSTPT) %>%
    select(-(starts_with("VS")), VSSTRESN) # nolint


  expected <- bind_rows(face, expected1)

  actual <- derive_fever_records(
    dataset = face,
    dataset_source = vs,
    filter_source = VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP",
    faobj = "FEVER"
  )

  expect_dfs_equal(actual,
    expected,
    keys = c("USUBJID", "FAOBJ", "FATESTCD", "FATEST", "FATPT")
  )
})



## Test 2: How the actual dataset is generated if FAOBJ="FEVER", if the FEVER records are  in FACE

test_that('derive_fever_records Test 2: How the actual dataset is generated
  if FAOBJ="FEVER", if the FEVER records are  in FACE', {
  face <- tibble::tribble(
    ~USUBJID, ~FAOBJ, ~FATESTCD, ~FACAT, ~FASCAT, ~FATPT, ~FAORRES, ~FASTRESC,
    "ABC101", "REDNESS", "SEV", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 1",
    NA, NA,
    "ABC101", "REDNESS", "DIAM", "REACTOGENICITY", "ADMINISTRATIVE SITE", "DAY 2",
    NA, NA,
    "ABC101", "VOMITTING", "SEV", "REACTOGENICITY", "SYSTEMIC", "DAY 1", NA, NA,
    "ABC101", "FEVER", "OCCUR", "REACTOGENICITY", "SYSTEMIC", "DAY 3", "Y", "Y",
  )

  vs <- tibble::tribble(
    ~USUBJID, ~VSTESTCD, ~VSCAT, ~VSSTRESN, ~VSSTRESU, ~VSTPT,
    "ABC101", "TEMP", "REACTOGENICITY", 38.3, "C", "DAY 1",
    "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 2",
    "ABC101", "TEMP", "REACTOGENICITY", 36, "C", "DAY 3",
    "ABC101", "TEMP", "REACTOGENICITY", 37, "C", "DAY 4",
    "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 5",
    "ABC101", "TEMP", "REACTOGENICITY", 39, "C", "DAY 6",
    "ABC101", "TEMP", "REACTOGENICITY", 38, "C", "DAY 7"
  )

  expected <- face

  actual <- derive_fever_records(
    dataset = face,
    dataset_source = vs,
    filter_source = VSCAT == "REACTOGENICITY" & VSTESTCD == "TEMP",
    faobj = "FEVER"
  )
  testthat::expect_equal(actual, expected)
})
