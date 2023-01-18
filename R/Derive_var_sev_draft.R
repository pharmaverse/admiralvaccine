#' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Title: DERIVE_PARAM_SEV
#' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#' @description                                                                +
#' This `derive_var_sev` function` is used to find the severity from Diameter  + 
#' for the events which has the AVALC/AVAL as diameter.                        +
#' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' @param dataset 
#' The Input data frame for this function
#' required variables for Input data freame :USUBJID,ATPTREF(FADTC),
#' PARCAT2(FASCAT), AVAL(FASTRESC),AVALC(FASTRESN),FAOBJ,FATESTCD,FATEST.
#' 
#' @param testcd_diam (FATESTCD)
#' Pass the Diameter `testcd` value with quotes which will helps to filter the
#' Diameter records convert into sev.
#' 
#' @param testcd_sev
#' Modify the severity code as per user need. default: FATESTCD will be "SEV".
#' 
#' @param test_sev
#' To modify the diameter FATEST to severity FATEST. 
#' default: FATEST will be "Severity/Intensity".
#'
#' @param none 
#' This `none` and following arguments will be used to pass the 
#' conditions as a vector(like an unequal Class interval) to derive the AVALC 
#' variable for given `faobj`.
#' this is how we have to pass and the conditions which was given inside the code.
#'  none = c(0,2), `none[1] <= AVAL & AVAL <= none[2]`
#'  mild = c(2,5), `mild[1] < AVAL & AVAL <= mild[2]`
#'  mod = c(5,10),`mod[1] < AVAL & AVAL <= mod[2]`
#'   sev = c(10)) `sev < AVAL  `
#'
#' @param mild 
#' @param mod 
#' @param sev 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @return Data frame which has Maxsev records with DTYPE as "MAXIMUM", Derived 
#' AVAL and AVALC and it will be appended with the Input data set. 
#' @export 
#'
#' @author ARJUN R
#'
#' @examples
#' modifying the data set
#' input <- derive_var_sev(face %>% filter(FACAT=="REACTOGENICITY") %>% 
#' mutate(
#'   AVAL = FASTRESN,
#'   AVALC = FASTRESC,
#'   ATPTREF = FATPTREF,
#'   PARCAT2 = FASCAT
#' ))
# output <-derive_var_sev(
#   dataset =  input,
#   testcd_diam = "DIAMETER",
#   faobj = c("REDNESS","SWELLING"),
#   testcd_sev = "SEV",
#   test_sev = "Severity/Intensity",
#   none = c(0,2),
#   mild = c(2,5),
#   mod = c(5,10),
#   sev = c(10))
################################################################################
#required packages
library(admiraldev)
library(tidyverse)
library(dplyr)
################################################################################
derive_var_sev <- function(
  dataset = NULL,
  testcd = NULL,
  faobj = NULL,
  testcd_sev = NULL,
  test_sev = NULL,
  none = NULL,
  mild = NULL,
  mod = NULL,
  sev = NULL
  ){

#-------------------------------------------------------------------------------
# assertion checks
#-------------------------------------------------------------------------------  
  assert_data_frame(dataset,
                    required_vars = vars(USUBJID, PARCAT2, AVAL, AVALC, 
                                         FAOBJ, FATEST, FATESTCD))
  
  assert_numeric_vector(arg = c(none,mild,mod,sev),optional = F)
  
  assert_character_vector(arg=c(testcd_sev,testcd_sev,testcd),optional=F)
  
#-------------------------------------------------------------------------------
# Checking & Removing the records which has severity records for the given FAOBJ
#-------------------------------------------------------------------------------  
  diam <- dataset %>% filter(FAOBJ %in% faobj)
  
  if(testcd_sev %in% diam$FATESTCD){
    
    Input <- dataset %>% filter(FATESTCD != testcd_sev & !(FAOBJ %in% faobj))
    
    
  }else{
    Input <- dataset
  }
#-------------------------------------------------------------------------------
# Replacing FATESTCD and FATEST for Diameter with Severity
#-------------------------------------------------------------------------------  
  if(testcd %in% diam$FATESTCD){
    
    Input <- dataset
    sev <- Input %>% filter(FATESTCD == testcd & FAOBJ %in% faobj)%>%
      mutate(
        FATESTCD = testcd_sev,
        FATEST = test_sev,
        #Deriving AVALC
        AVALC = if_else(none[1]<= AVAL & AVAL <= none[2],"NONE",
                        if_else(mild[1] < AVAL & AVAL <= mild[2],"MILD",
                                if_else(mod[1] < AVAL & AVAL <= mod[2],"MODERATE",
                                        if_else(sev[1] < AVAL,"SEVERE", AVALC)))),
        #Deriving AVAL
        AVAL = case_when(
          AVALC == "NONE" ~ 0,
          AVALC == "MILD" ~ 1,
          AVALC == "MODERATE" ~ 2,
          AVALC == "SEVERE" ~ 3
        )
      )
    #binding with input data set
    finalsev <- rbind(sev,Input)
    return(finalsev)
    
    
  }else{
    print(paste0(testcd, "doesn't exist in the subsetted record"))
  }
}

################################################################################
#demo
# input <- face %>% filter(FACAT=="REACTOGENICITY") %>%
# mutate(
# AVAL = FASTRESN,
# AVALC = FASTRESC,
# ATPTREF = FATPTREF,
# PARCAT2 = FASCAT)
# 
# output <-derive_var_sev(
#   dataset =  input,
#   testcd = "DIAMETER",
#   faobj = c("REDNESS","SWELLING"),
#   testcd_sev = "SEV",
#   test_sev = "Severity/Intensity",
#   none = c(0,2),
#   mild = c(2,5),
#   mod = c(5,10),
#   sev = c(10))
################################################################################