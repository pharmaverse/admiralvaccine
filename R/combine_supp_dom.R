#' combine_supp_dom
#' 
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#' @description                                                                   +
#' Combine original domain with its supplemental domain, e.g., IS with SUPPIS.    +
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' 
#' @param dataset 
#' Input dataset to which you want merge variables from its supplemental dataset.
#' Variable requested: STUDYID, USUBJID.
#' 
#' @param supp_dataset 
#' Supplemental dataset on which there are relevant variables you want to merge to
#' its original dataset. 
#' Variable requested: STUDYID, USUBJID, IDAVR, QNAM, QVAL, IDVARVAL.  
#' 
#' 
#' 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @return dataset containing both info from original and supplemental datasets
#' variables.
#'
#' @author Federico Baratin
#'
#' @examples
#' 
# combine_supp_dom(
# dataset = is,
# supp_dataset = suppis)
#
#################################################################################




combine_supp_dom <- function (dataset, supp_dataset) 
{
  
  assert_data_frame(dataset, required_vars = vars(STUDYID,USUBJID))
  assert_data_frame(supp_dataset, required_vars = vars(STUDYID,USUBJID,IDVAR,QNAM,QVAL,IDVARVAL))
  
  if(supp_dataset$RDOMAIN[1] != "DM"){
    
    dataset <- dataset %>% mutate_if(is.character, list(~ na_if(.,"")))
    supp_dataset <- supp_dataset %>% mutate_if(is.character, list(~ na_if(.,""))) %>%
      select(-QLABEL)
    
    idvar <- supp_dataset$IDVAR[[1]]
    
    supp_dataset_tr <- supp_dataset %>%
      pivot_wider(names_from = QNAM, values_from = QVAL) %>%
      mutate(`:=`(!!idvar, as.numeric(IDVARVAL)))
    
    output <- left_join(dataset, supp_dataset_tr, by = c("STUDYID", "USUBJID", idvar))
    
  }
  
  else if(supp_dataset$RDOMAIN[1] == "DM"){
    
    dataset <- dataset %>% mutate_if(is.character, list(~ na_if(.,"")))
    supp_dataset <- supp_dataset %>% mutate_if(is.character, list(~ na_if(.,""))) %>%
      select(-QLABEL)
    
    supp_dataset_tr <- supp_dataset %>%
      pivot_wider(names_from = QNAM, values_from = QVAL)
    
    output <- left_join(dataset, supp_dataset_tr, by = c("STUDYID", "USUBJID"))
  }
  
  return(output)
}