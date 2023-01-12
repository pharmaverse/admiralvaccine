#' Title: DERIVE_PARAM_MAXSEV 
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#' @description                                                                 +
#' This derive_param_maxsev function is used to derive the maximum severity for +
#'  each administrative and Systemic events.                                    + 
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @details 
#' 
#' Pre_processing:
#'    
#' Severity records for Administrative and systemic events will be filtered 
#' from the input data set and AVAL will be derived in a data_frame [pp] 
#' 
#' Maximum severity derivation:   
#'     
#' Once the pre-processing is done. The maximum severity records will be derived
#' from the  AVAL by USUBJID, group(ex.FAOBJ)[by_var] and period(ATPTREF)
#' [period_var] variables. 
#' Below variables will be created with the values mentioned below.
#' [DTYPE = "MAXIMUM", FATESTCD = "MAXSEV" and FATEST = "MAXIMUM SEVERITY".]
#' 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' @param dataset 
#' The Input data frame for this function
#' required variables for Input data freame :USUBJID,ATPTREF(FADTC),
#' PARCAT2(FASCAT), AVAL(FASTRESC),AVALC(FASTRESN),FAOBJ,FATESTCD,FATEST.
#' 
#' @param testcd_sev 
#' In testcd_sev, Pass the testcd in the quotes ("SEV") which is representing 
#' the severity records.if you have more than one testcd, pass it as a list in
#' the [testcd_sev] arugument.  
#' 
#' @param by_var
#' 
#' In by_var we have to pass the FABOJ which will work as the second grouping 
#' variable after USUBJID.
#'  
#' @param period_var 
#' 
#' In period_var we have to pass the Period variable which representing the 
#' each observation period for each FAOBJ. which will be the third grouping 
#' variable after FAOBJ. (eg: ATPTREF)
#' 
#' 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @return Dataframe which has Maxsev records (with DTYPE as "MAXIMUM", Derived AVAL
#' and AVALC and some grouping vars and updated FATESTCD and FATEST.) that would
#' have appended with the Input data set. 
#' @export 
#'
#' @author ARJUN R
#'
#' @examples
#' 
# derive_param_maxsev(
# testcd_sev = "SEV"
# dataset = face1,
# by_var = FAOBJ,
# period_var = ATPTREF)
#
#################################################################################

derive_param_maxsev <- function(
  dataset = NULL,
  testcd_sev = "SEV",
  by_var = NULL,
  period_var = NULL
){
  #vars check
  assert_data_frame(dataset,
                    required_vars = vars(USUBJID, PARCAT2, ATPTREF, AVAL, AVALC, 
                                         FAOBJ, FATEST, FATESTCD))
  assert_character_vector(testcd_sev,optional = FALSE)
  
  by_var <- enquo(by_var)
  period_var <- enquo(period_var)
  
  #filtering the severity record     
  
  pp <- dataset %>% filter(FATESTCD %in% testcd_sev & 
                             PARCAT2  %in% c("ADMINISTRATION SITE", "SYSTEMIC")) %>% 
    mutate(
      #AVAL derivation
      
      AVAL = case_when(
        AVALC == "NONE" ~ 0,
        AVALC == "MILD" ~ 1,
        AVALC == "MODERATE" ~ 2,
        AVALC == "SEVERE" ~ 3,
        AVALC == "GRADE4" ~ 4 )
    )
  
  #deriving the maximum severity
  
  maxsev <- pp %>%
    group_by(USUBJID, !!by_var, !!period_var) %>% 
    summarise(AVAL = max(AVAL))%>% 
    mutate(
      AVALC = case_when(
        AVAL == 0 ~ "NONE",
        AVAL == 1 ~ "MILD",
        AVAL == 2 ~ "MODERATE",
        AVAL == 3 ~ "SEVERE"
      ),
      DTYPE = "MAXIMUM",
      FATESTCD = "MAXSEV",
      FATEST = "Maximum Severity"
    ) 
  
  return(maxsev)
}

#######################################################################################
#To check

output <- derive_param_maxsev(
  dataset = face1,
  testcd_sev = "SEV",
  by_var = FAOBJ,
  period_var = ATPTREF)


#############################################################################################