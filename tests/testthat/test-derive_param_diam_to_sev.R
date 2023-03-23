library(admiraldev)
library(rlang)
library(tidyverse)
library(diffdf)
library(testthat)


## Test 1: derive `FATEST`,`FATESTCD` indicating severity for the event `REDNESS`

test_that("derive_param_diam_to_sev Test 1: derive `FATEST`,`FATESTCD` indicating
          severity for the event `REDNESS`",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 7.5,  "7.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 3.5,  "3.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 2,    "2",      "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 11,  "11",    "VACCINATION 1", "Diameter","DIAMETER"
            )

            format_avalc <- function(x){
              case_when(
                between(x,0,2) ~ 'NONE',
                between(x,2,5) ~ 'MILD',
                between(x,5,10) ~ 'MODERATE',
                x>10 ~ 'SEVERE'
              )
            }

            expected1 <- input %>%
              mutate(FATEST = "Severity/Intensity",
                     FATESTCD = "SEV",
                     AVALC = format_avalc(AVAL),
                     DTYPE = "DERIVED")

            format_aval <- function(x){
              case_when(
                x=='NONE' ~ 0,
                x=='MILD' ~ 1,
                x=='MODERATE' ~ 2,
                x=='SEVERE' ~ 3
              )
            }
            expected_output <- bind_rows(input,expected1 %>%
                                           mutate(AVAL=format_aval(AVALC)))

            actual_output <- derive_param_diam_to_sev(
              dataset=input,
              filter_diam = "DIAMETER",
              filter_faobj = c('REDNESS'),
              testcd_sev = "SEV",
              test_sev = "Severity/Intensity",
              none = c(0,2),
              mild = c(2,5),
              mod = c(5,10),
              sev = 10)

            expect_dfs_equal(
              expected_output,
              actual_output,
              keys = c('USUBJID','FAOBJ','AVAL','AVALC','ATPTREF','FATEST','FATESTCD')
            )

          })

## Test 2: derive `FATEST`,`FATESTCD` indicating severity for the event `REDNESS` & `SWELLING`

test_that("derive_param_diam_to_sev Test 2: derive `FATEST`,`FATESTCD` indicating
          severity for the event `REDNESS` & `SWELLING`",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 1.5,   "1.5",  "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 11,    "11",   "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "SWELLING", 6.5,  "7.5",  "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "SWELLING", 4.5,  "3.5",  "VACCINATION 1", "Diameter","DIAMETER"
            )

            format_avalc <- function(x){
              case_when(
                between(x,0,2) ~ 'NONE',
                between(x,2,5) ~ 'MILD',
                between(x,5,10) ~ 'MODERATE',
                x>10 ~ 'SEVERE'
              )
            }

            expected1 <- input %>%
              mutate(FATEST = "Severity",
                     FATESTCD = "SEV",
                     AVALC = format_avalc(AVAL),
                     DTYPE = "DERIVED")

            format_aval <- function(x){
              case_when(
                x=='NONE' ~ 0,
                x=='MILD' ~ 1,
                x=='MODERATE' ~ 2,
                x=='SEVERE' ~ 3
              )
            }
            expected_output <- bind_rows(input,expected1 %>%
                                           mutate(AVAL=format_aval(AVALC)))

            actual_output <- derive_param_diam_to_sev(
              dataset=input,
              filter_diam = "DIAMETER",
              filter_faobj = c('REDNESS','SWELLING'),
              testcd_sev = "SEV",
              test_sev = "Severity",
              none = c(0,2),
              mild = c(2,5),
              mod = c(5,10),
              sev = 10)

            expect_dfs_equal(
              expected_output,
              actual_output,
              keys = c('USUBJID','FAOBJ','AVAL','AVALC','ATPTREF','FATEST','FATESTCD')
            )
          })

##Test 3: Check if the arguments `none`,`mild`,`moderate`,`sev` works correctly"

test_that("derive_param_diam_to_sev Test 3: Check if the arguments `none`,
          `mild`,`moderate`,`sev` works correctly",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 7.5,  "7.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 3.5,  "3.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 2,    "2",      "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 10,   "10",    "VACCINATION 1", "Diameter","DIAMETER"
            )

            format_avalc <- function(x){
              case_when(
                between(x,0,3) ~ 'NONE',
                between(x,3,6) ~ 'MILD',
                between(x,6,9) ~ 'MODERATE',
                x>9 ~ 'SEVERE'
              )
            }

            expected1 <- input %>%
              mutate(FATEST = "Severity/Intensity",
                     FATESTCD = "SEV",
                     AVALC = format_avalc(AVAL),
                     DTYPE = "DERIVED")

            format_aval <- function(x){
              case_when(
                x=='NONE' ~ 0,
                x=='MILD' ~ 1,
                x=='MODERATE' ~ 2,
                x=='SEVERE' ~ 3
              )
            }
            expected_output <- bind_rows(input,expected1 %>%
                                           mutate(AVAL=format_aval(AVALC)))

            actual_output <- derive_param_diam_to_sev(
              dataset=input,
              filter_diam = "DIAMETER",
              filter_faobj = c('REDNESS'),
              testcd_sev = "SEV",
              test_sev = "Severity/Intensity",
              none = c(0,3),
              mild = c(3,6),
              mod = c(6,9),
              sev = 9)

            expect_dfs_equal(
              expected_output,
              actual_output,
              keys = c('USUBJID','FAOBJ','AVAL','AVALC','ATPTREF','FATEST','FATESTCD')
            )
          })

## Test 4: Check if the input dataset has severity records and remove those records correctly

test_that("derive_param_diam_to_sev Test 4: Check if the input dataset has
          severity records and remove those records correctly",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 1.5,   "1.5",  "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 11,    "11",   "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "SWELLING", 6.5,  "7.5",  "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "SWELLING", 4.5,  "3.5",  "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 1.5,   "NONE",  "VACCINATION 1", "Severity","SEV",
              "XYZ1001", "REDNESS", 11,    "SEVERE",   "VACCINATION 1", "Severity","SEV",
              "XYZ1001", "SWELLING", 6.5,  "MODERATE",  "VACCINATION 1", "Severity","SEV",
              "XYZ1001", "SWELLING", 4.5,  "MILD",  "VACCINATION 1", "Severity","SEV"
            )

            format_avalc <- function(x){
              case_when(
                between(x,0,2) ~ 'NONE',
                between(x,2,5) ~ 'MILD',
                between(x,5,10) ~ 'MODERATE',
                x>10 ~ 'SEVERE'
              )
            }

            input <- input %>%
              filter(FATESTCD != "SEV" & FAOBJ %in% c('REDNESS','SWELLING'))

            expected1 <- input %>%
              mutate(FATEST = "Severity",
                     FATESTCD = "SEV",
                     AVALC = format_avalc(AVAL),
                     DTYPE = "DERIVED")

            format_aval <- function(x){
              case_when(
                x=='NONE' ~ 0,
                x=='MILD' ~ 1,
                x=='MODERATE' ~ 2,
                x=='SEVERE' ~ 3
              )
            }
            expected_output <- bind_rows(input,expected1 %>%
                                           mutate(AVAL=format_aval(AVALC)))

            actual_output <- derive_param_diam_to_sev(
              dataset=input,
              filter_diam = "DIAMETER",
              filter_faobj = c('REDNESS','SWELLING'),
              testcd_sev = "SEV",
              test_sev = "Severity",
              none = c(0,2),
              mild = c(2,5),
              mod = c(5,10),
              sev = 10)

            expect_dfs_equal(
              expected_output,
              actual_output,
              keys = c('USUBJID','FAOBJ','AVAL','AVALC','ATPTREF','FATEST','FATESTCD')
            )
          })

## Test 5: Check if the arguments `test_sev`,`testcd_sev` works correctly

test_that("derive_param_diam_to_sev Test 5: Check if the arguments `test_sev`,
          `testcd_sev` works correctly",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 7.5,  "7.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 3.5,  "3.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 2,    "2",      "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 11,  "11",    "VACCINATION 1", "Diameter","DIAMETER"
            )

            format_avalc <- function(x){
              case_when(
                between(x,0,2) ~ 'NONE',
                between(x,2,5) ~ 'MILD',
                between(x,5,10) ~ 'MODERATE',
                x>10 ~ 'SEVERE'
              )
            }

            expected1 <- input %>%
              mutate(FATEST = "Severity/Intensity/Sev",
                     FATESTCD = "SEVERITY/SEV",
                     AVALC = format_avalc(AVAL),
                     DTYPE = "DERIVED")

            format_aval <- function(x){
              case_when(
                x=='NONE' ~ 0,
                x=='MILD' ~ 1,
                x=='MODERATE' ~ 2,
                x=='SEVERE' ~ 3
              )
            }
            expected_output <- bind_rows(input,expected1 %>%
                                           mutate(AVAL=format_aval(AVALC)))

            actual_output <- derive_param_diam_to_sev(
              dataset=input,
              filter_diam = "DIAMETER",
              filter_faobj = c('REDNESS'),
              testcd_sev = "SEVERITY/SEV",
              test_sev = "Severity/Intensity/Sev",
              none = c(0,2),
              mild = c(2,5),
              mod = c(5,10),
              sev = 10)

            expect_dfs_equal(
              expected_output,
              actual_output,
              keys = c('USUBJID','FAOBJ','AVAL','AVALC','ATPTREF','FATEST','FATESTCD')
            )
          })

## Test 6: error is issued if the `filter_diam` to be filtered is not in the input dataset

test_that("derive_param_diam_to_sev Test 6: error is issued if the `filter_diam`
          to be filtered is not in the input dataset",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 7.5,  "7.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 3.5,  "3.5",    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 2,    "2",      "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 11,  "11",    "VACCINATION 1", "Diameter","DIAMETER"
            )

            expect_error(
              derive_param_diam_to_sev(
                dataset=input,
                filter_diam = "DIAM",
                filter_faobj = c('REDNESS'),
                testcd_sev = "SEVERITY/SEV",
                test_sev = "Severity/Intensity/Sev",
                none = c(0,2),
                mild = c(2,5),
                mod = c(5,10),
                sev = 10
              ),
              regexp = paste(
                "DIAM doesn't exist in the filtered record")
            )

          })

## Test 7: error is issued if AVALC is not a character vector in input dataset

test_that("derive_param_diam_to_sev Test 7: error is issued if AVALC is not
          a character vector in input dataset",{
            input <- tribble(
              ~USUBJID,  ~FAOBJ,    ~AVAL,  ~AVALC,  ~ATPTREF,       ~FATEST, ~FATESTCD,
              "XYZ1001", "REDNESS", 7.5,  7.5,    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 3.5,  3.5,    "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 2,    2,      "VACCINATION 1", "Diameter","DIAMETER",
              "XYZ1001", "REDNESS", 11,  11,    "VACCINATION 1", "Diameter","DIAMETER"
            )

            expect_error(
              derive_param_diam_to_sev(
                dataset=input,
                filter_diam = "DIAMETER",
                filter_faobj = c('REDNESS'),
                testcd_sev = "SEVERITY/SEV",
                test_sev = "Severity/Intensity/Sev",
                none = c(0,2),
                mild = c(2,5),
                mod = c(5,10),
                sev = 10
              ),
              regexp = paste(
                "AVALC must be a character vector")
            )
          })

