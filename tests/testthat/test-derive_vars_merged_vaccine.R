test_that("derive_vars_merged_vaccine Test 1: Merging EXTRT variable from EX to FACE", {
  face <- tibble::tribble(
    ~USUBJID, ~FACAT, ~FASCAT, ~FATESTCD, ~FAOBJ, ~FATEST, ~FALOC, ~FALAT, ~FATPTREF,
    "ABC101", "REACTO", "ADMINISTRATION SITE", "SEV", "Redness", "Severity", "ARM",
    "RIGHT", "VAC 1",
    "ABC101", "REACTO", "ADMINISTRATION SITE", "DIAMETER", "Redness", "Diameter", "ARM",
    "LEFT", "VAC 1",
    "ABC101", "REACTO", "ADMINISTRATION SITE", "DIAM", "Redness", "Diameter",
    NA, NA, "VAC 2",
    "ABC101", "REACTO", "SYSTEMIC", "OCCUR", "Fatigue", "Occurrence", "LEG", "LEFT",
    "VAC 3",
    "ABC101", "REACTO", "ADMINISTRATION SITE", "OCCUR", "Erythema", "Occurrence", "LEG",
    "LEFT", "VAC 3",
    "ABC101", "REACTO", "ADMINISTRATION SITE", "SEV", "Swelling", "Severity", NA, NA,
    "VAC 4",
    "ABC101", "REACTO", "ADMINISTRATION SITE", "OCCUR", "Swelling", "Occurrence", NA, NA,
    "VAC 4",
    "ABC102", "REACTO", "ADMINISTRATION SITE", "OCCUR", "Swelling", "Occurrence", NA, NA,
    "VAC 1"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXTPTREF, ~VISIT, ~EXLOC, ~EXLAT, ~EXDOSE,
    "ABC101", "2015-01-10", 1, "DRUG A", "VAC 1", "VISIT 1", "ARM", "RIGHT", 20,
    "ABC101", "2015-01-11", 2, "DRUG A", "VAC 2", "VISIT 2", NA, NA, 30,
    "ABC101", "2015-01-12", 3, "DRUG B", "VAC 3", "VISIT 3", "LEG", "LEFT", 25,
    "ABC101", "2015-01-13", 4, "DRUG C", "VAC 4", "VISIT 4", NA, NA, 30,
    "ABC102", "2015-01-13", 1, "DRUG B", "VAC 1", "VISIT 5", NA, NA, 10
  )

  face1 <- face %>%
    mutate(LOC = FALOC, LAT = FALAT, TPTREF = FATPTREF)

  ex1 <- ex %>%
    mutate(LOC = EXLOC, LAT = EXLAT, TPTREF = EXTPTREF) %>%
    select(-c("VISITNUM", "VISIT", "EXLOC", "EXLAT", "EXSTDTC", "EXTPTREF"))

  expected <- left_join(face1, ex1, by = c("USUBJID", "LOC", "LAT", "TPTREF"), keep = FALSE) %>%
    select(-c("LOC", "LAT", "TPTREF"))

  actual <- derive_vars_merged_vaccine(
    dataset = face,
    dataset_ex = ex,
    by_vars_sys = exprs(USUBJID, FATPTREF = EXTPTREF),
    by_vars_adms = exprs(USUBJID, FATPTREF = EXTPTREF, FALOC = EXLOC, FALAT = EXLAT),
    ex_vars = exprs(EXTRT, EXDOSE)
  )
  expect_dfs_equal(actual, expected, keys = c(
    "USUBJID", "FAOBJ", "FATESTCD", "FATPTREF",
    "FALOC", "FALAT"
  ))
})


test_that("derive_vars_merged_vaccine Test 2: Check if warning is raised when
          there are multiple vaccination in same ", {
  face <- tibble::tribble(
    ~STUDYID, ~DOMAIN, ~USUBJID, ~FACAT, ~FASCAT, ~FATESTCD, ~FAOBJ, ~FATEST, ~FALOC, ~FALAT,
    ~FATPTREF, ~FASEQ,
    "ABC", "FACE", "ABC101", "REACTO", "ADMINISTRATION SITE", "SEV", "Redness", "Severity", "ARM",
    "RIGHT", "VAC 1", 1,
    "ABC", "FACE", "ABC101", "REACTO", "ADMINISTRATION SITE", "DIAMETER", "Redness", "Diameter",
    "ARM", "LEFT", "VAC 1", 2,
    "ABC", "FACE", "ABC101", "REACTO", "ADMINISTRATION SITE", "DIAM", "Redness", "Diameter",
    NA, NA, "VAC 2", 3,
    "ABC", "FACE", "ABC101", "REACTO", "SYSTEMIC", "OCCUR", "Fatigue", "Occurrence",
    "LEG", "LEFT", "VAC 3", 5,
    "ABC", "FACE", "ABC101", "REACTO", "ADMINISTRATION SITE", "OCCUR", "Erythema",
    "Occurrence", "LEG", "LEFT", "VAC 3", 6,
    "ABC", "FACE", "ABC101", "REACTO", "ADMINISTRATION SITE", "SEV", "Swelling",
    "Severity", NA, NA, "VAC 4", 7,
    "ABC", "FACE", "ABC101", "REACTO", "ADMINISTRATION SITE", "OCCUR", "Swelling",
    "Occurrence", NA, NA, "VAC 4", 8,
    "ABC", "FACE", "ABC102", "REACTO", "ADMINISTRATION SITE", "OCCUR", "Swelling",
    "Occurrence", NA, NA, "VAC 1", 1
  )
  ex <- tibble::tribble(
    ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXTPTREF, ~VISIT, ~EXLOC, ~EXLAT, ~EXDOSE,
    "ABC101", "2015-01-10", 1, "DRUG A", "VAC 1", "VISIT 1", "ARM", "RIGHT", 20,
    "ABC101", "2015-01-11", 2, "DRUG A", "VAC 2", "VISIT 1", NA, NA, 30,
    "ABC101", "2015-01-12", 3, "DRUG B", "VAC 3", "VISIT 3", "LEG", "LEFT", 25,
    "ABC101", "2015-01-13", 4, "DRUG C", "VAC 4", "VISIT 4", NA, NA, 30,
    "ABC102", "2015-01-13", 1, "DRUG B", "VAC 1", "VISIT 5", NA, NA, 10
  )


  expect_warning(
    derive_vars_merged_vaccine(
      dataset = face,
      dataset_ex = ex,
      by_vars_sys = exprs(USUBJID, FATPTREF = EXTPTREF),
      by_vars_adms = exprs(USUBJID, FATPTREF = EXTPTREF, FALOC = EXLOC, FALAT = EXLAT),
      ex_vars = exprs(EXTRT, EXDOSE)
    ),
    regexp = paste(
      "Subjects have multiple vaccinations at same visit"
    )
  )
})
