#' derive_vars_flag
#' 
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#' @description                                                                   +
#' Derive charscater flag variable. Relative numeric flag and label variables are +
#' optionals.                                                                     +
#' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' 
#' 
#' @param dataset 
#' Input dataset in which you want to create the new flag variable.
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
#################################################################################ù




test <- function(dataset,var,label_var,whrcl,rule){
  
  whrcl <- assert_filter_cond(enquo(whrcl))
  rule <- assert_filter_cond(enquo(rule))
  
  if (!grepl("CRIT",var) & !grepl("ANL",var)) {
    abort("You need to insert CRIT or ANL as input variables")
  }
  
  if (grepl("CRIT",var)){
    
    var_char <- paste0(var,"FL")
    var_num <- paste0(var,"FN")
    
    data <- dataset %>%
      filter({{whrcl}}) %>%
      mutate(`:=` (!!var_char,
                   if_else(!!rule, "Y", "N")),
             
             `:=` (!!var_num,
                   if_else(!!rule, 1, 0)),
             
             `:=` (!!var,label_var)
      ) %>%
      select(STUDYID,USUBJID,VISITNUM,ISTESTCD,starts_with("CRIT"))
    
    data2 <- left_join(dataset, data, by = c("STUDYID","USUBJID","VISITNUM","ISTESTCD"))
  }
  
  
  if(grepl("ANL",var)){
    
    var_char <- paste0(var,"FL")
    
    data <- dataset %>%
      filter({{whrcl}}) %>%
      mutate(`:=` (!!var_char,
                   if_else(!!rule, "Y", "N"))
      ) %>%
      select(STUDYID,USUBJID,VISITNUM,ISTESTCD,starts_with("ANL"))
    
    data2 <- left_join(dataset, data, by = c("STUDYID","USUBJID","VISITNUM","ISTESTCD"))
  }
  
  
  return(data2)
}

prova2<-test(dataset = is,
            var = "CRIT1",
            label_var = "Value is good",
            whrcl = !is.na(ISSTRESN),
            rule = VISITNUM == 10)

prova<-test(dataset = is,
            var = "ANL01",
            whrcl = !is.na(ISSTRESN),
            rule = VISITNUM == 10)




###second way
test <- function(dataset,var,label_var,whrcl,rule){
  
  whrcl <- assert_filter_cond(enquo(whrcl))
  rule <- assert_filter_cond(enquo(rule))
  
  if (grepl("CRIT",var)){
    
    var_char <- paste0(var,"FL")
    var_num <- paste0(var,"FN")
    
    data <- dataset %>%
      mutate(`:=` (!!var_char,
                   case_when(!(!!whrcl) ~ as.character(NA),
                             !!rule & !!whrcl ~ "Y",
                             !(!!rule) & !!whrcl ~ "N")
      ),
      
      `:=` (!!var_num,
            case_when(!(!!whrcl) ~ as.numeric(NA),
                      !!rule & !!whrcl ~ 1,
                      !(!!rule) & !!whrcl ~ 0)
      ),
      
      `:=` (!!var,
            case_when(!(!!whrcl) ~ as.character(NA),
                      !!rule & !!whrcl ~ label_var,
                      !(!!rule) & !!whrcl ~ label_var
            )
      )
      ) 
  }
  
  
  if(grepl("ANL",var)){
    
    var_char <- paste0(var,"FL")
    
    data <- dataset %>%
      mutate(`:=` (!!var_char,
                   case_when(!(!!whrcl) ~ as.character(NA),
                             !!rule & !!whrcl ~ "Y",
                             !(!!rule) & !!whrcl ~ "N")
      )
      )
  }
  
  if (!grepl("CRIT",var) & !grepl("ANL",var)) {
    abort("You need to insert CRIT or ANL as input variables")
  }
  
  
  return(data)
}

prova<-test(dataset = is,
            var = "CRIT1",
            label_var = "Value is good",
            whrcl = !is.na(ISSTRESN),
            rule = VISITNUM == 10)
