library(admiraldev)
library(admiral)
library(rlang)
library(diffdf)
library(testthat)
library(tidyverse)
library(dplyr)
library(lubridate)

# testthat
testthat::test_that('testcase-1: Getting Vaccination dates from EX and check if output is
                    merged with ADSL',{
                      input<-tribble(
                        ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXLNKGRP,~VISIT,
                        "ABC001","2015-01-10",1,"DRUG A","VAC 1","VISIT 1",
                        "ABC001","2015-01-11",2,"DRUG A","VAC 2","VISIT 2",
                        "ABC001","2015-01-12",3,"DRUG B","VAC 3","VISIT 3",
                        "ABC001","2015-01-13",4,"DRUG B","VAC 4","VISIT 4",
                        "ABC002","2015-01-13",1,"DRUG B","VAC 1","VISIT 1")

                      adsl <-tribble(
                        ~USUBJID, ~SEX, ~AGE,
                        "ABC001","MALE",23,
                        "ABC002","FEMALE",26,
                      )

                      # Test 1: Getting Vaccination dates from EX and check if output is merged with
                      # ADSL
                      # Expected Output

                      temp <- input %>% group_by(USUBJID) %>%
                        pivot_wider(id_cols = USUBJID,
                                    names_from = VISITNUM,
                                    names_prefix = "VAX0",
                                    values_from = EXSTDTC)
                      colnames(temp) <- paste(colnames(temp),"DT",sep = "")
                      expected <- temp %>% rename("USUBJID" = "USUBJIDDT")
                      i = 1
                      while(i <=4){
                        col_name = paste(as.character("VAX0"),as.character(i),as.character("DT"),sep="")
                        expected[col_name] <- as.Date(expected[[col_name]], format =  "%Y-%m-%d")
                        i=i+1
                      }

                      expected <- left_join(
                        x = adsl,
                        y = expected,
                        by = c("USUBJID"),
                        keep = FALSE
                      )

                      # Actual Output
                      actual <- derive_vars_vaxdt(
                        dataset = input,
                        dataset_adsl = adsl,
                        by_vars = exprs(USUBJID,VISITNUM),
                        order = exprs(USUBJID,VISITNUM,VISIT,EXSTDTC)
                      )
                      expect_dfs_equal(actual, expected, keys = c('USUBJID'))
                    })


# testthat
testthat::test_that('testcase-2: Check if Vaccination date variables are getting created when multiple
                    vaccination given on same visit',{

                      input<-tribble(
                        ~USUBJID, ~EXSTDTC, ~VISITNUM, ~EXTRT, ~EXLNKGRP,~VISIT,
                        "ABC001","2015-01-10",1,"DRUG A","VAC 1","VISIT 1",
                        "ABC001","2015-01-10",1,"DRUG A","VAC 1","VISIT 1",
                        "ABC001","2015-01-12",2,"DRUG B","VAC 2","VISIT 2",
                        "ABC001","2015-01-13",3,"DRUG B","VAC 3","VISIT 3",
                        "ABC002","2015-01-13",1,"DRUG B","VAC 1","VISIT 1")
                      adsl <-tribble(
                        ~USUBJID, ~SEX, ~AGE,
                        "ABC001","MALE",23,
                        "ABC002","FEMALE",26,
                      )

                      # Test 2: Check if Vaccination date variables are getting created when multiple
                      # vaccination given on same visit

                      # Expected Output
                      temp <- input %>% group_by(USUBJID,VISITNUM,VISIT,EXSTDTC) %>%
                        distinct(USUBJID,VISITNUM,VISIT,EXSTDTC, .keep_all = TRUE) %>%
                        pivot_wider(id_cols = USUBJID,
                                    names_from = VISITNUM,
                                    names_prefix = "VAX0",
                                    values_from = EXSTDTC)
                      colnames(temp) <- paste(colnames(temp),"DT",sep = "")
                      expected <- temp %>% rename("USUBJID" = "USUBJIDDT")
                      i = 1
                      while(i <=3){
                        col_name = paste(as.character("VAX0"),as.character(i),as.character("DT"),sep="")
                        print(col_name)
                        expected[col_name] <- as.Date(expected[[col_name]], format =  "%Y-%m-%d")
                        i=i+1
                      }
                      expected <- left_join(
                        x = adsl,
                        y = expected,
                        by = c("USUBJID"),
                        keep = FALSE
                      )

                      # Actual Output
                      actual <- derive_vars_vaxdt(
                        dataset = input,
                        dataset_adsl = adsl,
                        by_vars = exprs(USUBJID,VISITNUM),
                        order = exprs(USUBJID,VISITNUM,VISIT,EXSTDTC)
                      )
                      expect_dfs_equal(actual, expected, keys = c('USUBJID'))
                    })

