## Test 1: Merging EXTRT variable from EX to FACE

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
    dataset_supp = NULL,
    dataset_suppex = NULL,
    by_vars_sys = exprs(USUBJID, FATPTREF = EXTPTREF),
    by_vars_adms = exprs(USUBJID, FATPTREF = EXTPTREF, FALOC = EXLOC, FALAT = EXLAT),
    ex_vars = exprs(EXTRT, EXDOSE)
  )
  expect_dfs_equal(actual, expected, keys = c(
    "USUBJID", "FAOBJ", "FATESTCD", "FATPTREF",
    "FALOC", "FALAT"
  ))
})

## Test 2: Check if supp datasets merged properly if they exist

test_that("derive_vars_merged_vaccine Test 2: Check if supp datasets merged
          properly if they exist", {
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
    ~STUDYID, ~DOMAIN, ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXTPTREF, ~VISIT, ~EXLOC, ~EXLAT,
    ~EXDOSE, ~EXSEQ,
    "ABC", "EX", "ABC101", "2015-01-10", 1, "DRUG A", "VAC 1", "VISIT 1", "ARM", "RIGHT", 20, 1,
    "ABC", "EX", "ABC101", "2015-01-11", 2, "DRUG A", "VAC 2", "VISIT 2", NA, NA, 30, 2,
    "ABC", "EX", "ABC101", "2015-01-12", 3, "DRUG B", "VAC 3", "VISIT 3", "LEG", "LEFT", 25, 3,
    "ABC", "EX", "ABC101", "2015-01-13", 4, "DRUG C", "VAC 4", "VISIT 4", NA, NA, 30, 4,
    "ABC", "EX", "ABC102", "2015-01-13", 1, "DRUG B", "VAC 1", "VISIT 5", NA, NA, 10, 1
  )

  suppface <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~RDOMAIN, ~IDVAR, ~IDVARVAL, ~QNAM, ~QVAL, ~QLABEL, ~QORIG,
    "ABC", "ABC101", "FACE", "FASEQ", 1, "CLTYP", "DAIRY", "Collection Type",
    "Predecessor",
    "ABC", "ABC101", "FACE", "FASEQ", 2, "CLTYP", "CRF", "Collection Type",
    "Predecessor"
  )

  suppex <- tibble::tribble(
    ~STUDYID, ~USUBJID, ~RDOMAIN, ~IDVAR, ~IDVARVAL, ~QNAM, ~QVAL, ~QLABEL, ~QORIG,
    "ABC", "ABC101", "EX", "EXSEQ", 1, "EXTDV", "N", "Temporary Delay of Vaccination",
    "ASSIGNED",
    "ABC", "ABC101", "EX", "EXSEQ", 2, "EXTDV", "Y", "Temporary Delay of Vaccination",
    "ASSIGNED"
  )

  temp <- suppface %>%
    pivot_wider(
      id_cols = c(USUBJID, IDVAR, IDVARVAL),
      names_from = QNAM,
      values_from = QVAL
    ) %>%
    mutate(FASEQ = IDVARVAL) %>%
    select(-c(IDVAR, IDVARVAL))
  facef <- left_join(face, temp, by = c("USUBJID", "FASEQ"), keep = FALSE)
  face1 <- facef %>%
    mutate(LOC = FALOC, LAT = FALAT, TPTREF = FATPTREF)

  tempex <- suppex %>%
    pivot_wider(
      id_cols = c(USUBJID, IDVAR, IDVARVAL),
      names_from = QNAM,
      values_from = QVAL
    ) %>%
    mutate(EXSEQ = IDVARVAL) %>%
    select(-c(IDVAR, IDVARVAL))
  exf <- left_join(ex, tempex, by = c("USUBJID", "EXSEQ"), keep = FALSE)

  ex1 <- exf %>%
    mutate(LOC = EXLOC, LAT = EXLAT, TPTREF = EXTPTREF) %>%
    select(-c(
      "VISITNUM", "VISIT", "EXLOC", "EXLAT", "EXSTDTC",
      "EXTPTREF", "STUDYID", "DOMAIN", "EXSEQ"
    ))

  admin <- face1 %>%
    filter(FASCAT == "ADMINISTRATION SITE")
  expected1 <- left_join(admin,
    ex1,
    by = c("USUBJID", "LOC", "LAT", "TPTREF"), keep = FALSE
  ) %>%
    select(-c("LOC", "LAT", "TPTREF"))

  sys <- face1 %>% filter(FASCAT == "SYSTEMIC")
  expected2 <- left_join(sys,
    ex1,
    by = c("USUBJID", "TPTREF"), keep = FALSE
  ) %>%
    select(-c("TPTREF", "LOC.x", "LOC.y", "LAT.x", "LAT.y"))

  expected <- bind_rows(expected1, expected2)


  actual <- derive_vars_merged_vaccine(
    dataset = face,
    dataset_ex = ex,
    dataset_supp = suppface,
    dataset_suppex = suppex,
    by_vars_sys = exprs(USUBJID, FATPTREF = EXTPTREF),
    by_vars_adms = exprs(USUBJID, FATPTREF = EXTPTREF, FALOC = EXLOC, FALAT = EXLAT),
    ex_vars = exprs(EXTRT, EXDOSE, EXTDV)
  )
  expect_identical(actual, expected)
})


## Test 3: Check if warning is raised when there are multiple vaccination in same visit

test_that("derive_vars_merged_vaccine Test 3: Check if warning is raised when
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
      dataset_supp = NULL,
      dataset_suppex = NULL,
      by_vars_sys = exprs(USUBJID, FATPTREF = EXTPTREF),
      by_vars_adms = exprs(USUBJID, FATPTREF = EXTPTREF, FALOC = EXLOC, FALAT = EXLAT),
      ex_vars = exprs(EXTRT, EXDOSE)
    ),
    regexp = paste(
      "Subjects have multiple vaccinations at same visit"
    )
  )
})
