options(repos = c(REPO_NAME = "https://rspm.pfizer.com/cran/latest"))
library(admiral.test)
library(admiraldev)
library(admiral)
library(diffdf)
library(rlang)

# testthat of derive_vars_merged_vaccine (test case 1)------------------------------------------------------------
testthat::test_that("derive_vars_merged_vaccine test case 1- merge the ex with face on records where FASCAT='SYSTEMIC'  ", {
  face1 <- tibble::tribble(
    ~USUBJID, ~FASCAT, ~FALAT, ~FADIR, ~FALOC,
    "ABC-1001", "SYSTEMIC", "", "", "",
    "ABC-1001", "ADMINISTRATION SITE", "RIGHT", "UPPER", "DELTOID MUSCLE",
    "ABC-1001", "ADMINISTRATION SITE", "LEFT", "LOWER", "ARM",
  )

  ex1 <- tibble::tribble(
    ~USUBJID, ~VISIT, ~EXTRT, ~EXDOSE, ~EXSEQ, ~EXLAT, ~EXDIR, ~EXLOC,
    "ABC-1001", "WEEK 1", "VACCINE A", 0.5, 1, "RIGHT", "UPPER", "DELTOID MUSCLE"
  )

  expect_sys <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>%
                mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF")) %>%
    filter(FASCAT == "SYSTEMIC")

  expect_ad <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    filter(FASCAT != "SYSTEMIC")

  expect_ex1 <- expect_ad %>% left_join(ex1, by = c("USUBJID" = "USUBJID", "FALAT" = "EXLAT"))

  expected <- plyr::rbind.fill(expect_sys, expect_ex1) %>%
    mutate(FAEVINTX = "") %>%
    select(-VISIT, -EXLAT, -EXDIR, -EXLOC)

  actual <- derive_vars_merged_vaccine(
    dataset = face1,
    dataset_ex = ex1,
    dataset_supp = NULL,
    dataset_suppex = NULL,
    ex_vars = rlang::exprs(EXTRT, EXDOSE, EXSEQ)
  )

  # merge the ex with face on records where FASCAT='SYSTEMIC'
  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FASCAT", "FALAT"))
})



# testthat of derive_vars_merged_vaccine (test case 2)------------------------------------------------------------
# merge the ex with face on records where FASCAT='ADMINISTRATION SITE' and FALAT='RIGHT'
testthat::test_that("derive_vars_merged_vaccine test case 2 - merge the ex with face on records where FASCAT='ADMINISTRATION SITE' and FALAT='RIGHT'", {
  face1 <- tibble::tribble(
    ~USUBJID, ~FASCAT, ~FALAT, ~FADIR, ~FALOC,
    "ABC-1001", "SYSTEMIC", NA_character_, NA, NA,
    "ABC-1001", "ADMINISTRATION SITE", "RIGHT", "UPPER", "DELTOID MUSCLE",
    "ABC-1001", "ADMINISTRATION SITE", "LEFT", "LOWER", "ARM",
  ) %>% mutate(across(FALAT, trimws))

  ex1 <- tibble::tribble(
    ~USUBJID, ~VISIT, ~EXTRT, ~EXDOSE, ~EXSEQ, ~EXLAT, ~EXDIR, ~EXLOC,
    "ABC-1001", "WEEK 1", "VACCINE A", 0.5, 1, "RIGHT", "UPPER", "DELTOID MUSCLE"
  )


  expect_sys <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>%
                mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF")) %>%
    filter(FASCAT == "SYSTEMIC") %>%
    select(-EXLAT, -EXDIR, -EXLOC)

  expect_ad <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>% mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF", "FALAT" = "EXLAT", "FADIR" = "EXDIR", "FALOC" = "EXLOC")) %>%
    filter(FASCAT != "SYSTEMIC")

  expected <- plyr::rbind.fill(expect_sys, expect_ad) %>%
    mutate(FAEVINTX = "") %>%
    select(-VISIT)

  actual <- derive_vars_merged_vaccine(
    dataset = face1,
    dataset_ex = ex1,
    dataset_supp = NULL,
    dataset_suppex = NULL,
    ex_vars = exprs(EXTRT, EXDOSE, EXSEQ)
  )


  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FASCAT", "FALAT"))
})





# testthat of derive_vars_merged_vaccine (test case 3)------------------------------------------------------------

# merge the ex with face on records where FASCAT='ADMINISTRATION SITE' and FALAT='LEFT'
testthat::test_that("derive_vars_merged_vaccine test case 3 - merge the face and ex domain on records where FASCAT='ADMINISTRATION SITE' and FALAT='LEFT' ", {
  face1 <- tibble::tribble(
    ~USUBJID, ~FASCAT, ~FALAT, ~FADIR, ~FALOC,
    "ABC-1001", "SYSTEMIC", NA_character_, NA, NA,
    "ABC-1001", "ADMINISTRATION SITE", "RIGHT", "UPPER", "DELTOID MUSCLE",
    "ABC-1001", "ADMINISTRATION SITE", "LEFT", "LOWER", "ARM",
  ) %>% mutate(across(FALAT, trimws))

  ex1 <- tibble::tribble(
    ~USUBJID, ~VISIT, ~EXTRT, ~EXDOSE, ~EXSEQ, ~EXLAT, ~EXDIR, ~EXLOC,
    "ABC-1001", "WEEK 1", "VACCINE A", 0.5, 1, "LEFT", "LOWER", "ARM"
  )


  expect_sys <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>%
                mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF")) %>%
    filter(FASCAT == "SYSTEMIC") %>%
    select(-EXLAT, -EXDIR, -EXLOC)

  expect_ad <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>%
                mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF", "FALAT" = "EXLAT", "FADIR" = "EXDIR", "FALOC" = "EXLOC")) %>%
    filter(FASCAT != "SYSTEMIC")

  expected <- plyr::rbind.fill(expect_sys, expect_ad) %>%
    mutate(FAEVINTX = "") %>%
    select(-VISIT)

  actual <- derive_vars_merged_vaccine(
    dataset = face1,
    dataset_ex = ex1,
    dataset_supp = NULL,
    dataset_suppex = NULL,
    ex_vars = exprs(EXTRT, EXDOSE, EXSEQ)
  )

  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FASCAT", "FALAT"))
})




# testthat of derive_vars_merged_vaccine (test case 4)------------------------------------------------------------

# merge the ex with face on records where FASCAT='ADMINISTRATION SITE' and FALAT in RIGHT & 'LEFT'
testthat::test_that("derive_vars_merged_vaccine test case 4 - if the ex domain has multiple vaccinations at same visit", {
  face1 <- tibble::tribble(
    ~USUBJID, ~FASCAT, ~FALAT, ~FADIR, ~FALOC,
    "ABC-1001", "SYSTEMIC", NA_character_, NA, NA,
    "ABC-1001", "ADMINISTRATION SITE", "RIGHT", "UPPER", "DELTOID MUSCLE",
    "ABC-1001", "ADMINISTRATION SITE", "LEFT", "LOWER", "ARM",
  ) %>% mutate(across(FALAT, trimws))

  ex1 <- tibble::tribble(
    ~USUBJID, ~VISIT, ~EXTRT, ~EXDOSE, ~EXSEQ, ~EXLAT, ~EXDIR, ~EXLOC,
    "ABC-1001", "WEEK 1", "VACCINE A", 0.5, 1, "LEFT", "LOWER", "ARM",
    "ABC-1001", "WEEK 1", "VACCINE A", 0.5, 1, "RIGHT", "UPPER", "DELTOID MUSCLE"
  )


  expect_sys <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>%
                mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF")) %>%
    filter(FASCAT == "SYSTEMIC") %>%
    select(-EXLAT, -EXDIR, -EXLOC)

  expect_ad <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>% mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF", "FALAT" = "EXLAT", "FADIR" = "EXDIR", "FALOC" = "EXLOC")) %>%
    filter(FASCAT != "SYSTEMIC")

  expected <- plyr::rbind.fill(expect_sys, expect_ad) %>% mutate(FAEVINTX = "")

  testthat::expect_warning(
    actual <- derive_vars_merged_vaccine(
      dataset = face1,
      dataset_ex = ex1,
      dataset_supp = NULL,
      dataset_suppex = NULL,
      ex_vars = exprs(EXTRT, EXDOSE, EXSEQ)
    ), "Subjects have multiple vaccinations at same visit"
  )
})


#  testthat of derive_vars_merged_vaccine (test case 5) -------------------

testthat::test_that("derive_vars_merged_vaccine test case 5 - merge the face and ex domain on records by USUBJID and FATPTREF if the FATPTREF, FALAT, FADIR and FALOC variables are not in the face domain ", {
  face1 <- tibble::tribble(
    ~USUBJID, ~FASCAT,
    "ABC-1001", "SYSTEMIC",
    "ABC-1001", "ADMINISTRATION SITE"
  )

  ex1 <- tibble::tribble(
    ~USUBJID, ~VISIT, ~EXTRT, ~EXDOSE, ~EXSEQ,
    "ABC-1001", "WEEK 1", "VACCINE A", 0.5, 1
  )


  expect_sys <- face1 %>%
    mutate(FATPTREF = "VACCINATION 1") %>%
    left_join(ex1 %>% mutate(EXTPTREF = "VACCINATION 1"), by = c("USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF")) %>%
    filter(FASCAT == "SYSTEMIC")

  expect_ad <- face1 %>%
    mutate(
      FATPTREF = "VACCINATION 1",
      FALAT = "",
      FADIR = "",
      FALOC = ""
    ) %>%
    filter(FASCAT != "SYSTEMIC") %>%
    left_join(ex1 %>% mutate(
      EXTPTREF = "VACCINATION 1",
      EXLAT = "",
      EXDIR = "",
      EXLOC = ""
    ), by = c(
      "USUBJID" = "USUBJID", "FATPTREF" = "EXTPTREF",
      "FALAT" = "EXLAT", "FADIR" = "EXDIR", "FALOC" = "EXLOC"
    ))

  expected <- plyr::rbind.fill(expect_sys, expect_ad) %>%
    mutate(FAEVINTX = "") %>%
    select(-VISIT) %>%
    mutate(
      FALAT = "",
      FADIR = "",
      FALOC = ""
    )

  actual <- derive_vars_merged_vaccine(
    dataset = face1,
    dataset_ex = ex1,
    dataset_supp = NULL,
    dataset_suppex = NULL,
    ex_vars = exprs(EXTRT, EXDOSE, EXSEQ)
  )

  expect_dfs_equal(actual, expected, keys = c("USUBJID", "FASCAT"))
})
