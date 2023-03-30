table_supp <- matrix(NA, nrow = 80, ncol = 10)

colnames(table_supp) <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QLABEL","QVAL","QORIG","QEVAL")

table_supp1 <- as.data.frame(table_supp)


table_supp1$USUBJID <-  c(rep("ABC-1001",8),rep("ABC-1002",8),rep("ABC-1003",8),rep("ABC-1004",8),
                          rep("ABC-1005",8),rep("ABC-1006",8),rep("ABC-1007",8),rep("ABC-1008",8),
                          rep("ABC-1009",8),rep("ABC-1010",8))


suppis1 <- table_supp1 %>%
  group_by(STUDYID,USUBJID) %>%
  mutate(IDVARVAL = row_number()) %>%
  ungroup() %>%
  mutate(STUDYID = "ABC",
         RDOMAIN = "IS",
         IDVAR = "ISSEQ",
         QNAM = "LOD",
         QLABEL = "Limit of Detection",
         IDVARVAL = as.character(IDVARVAL),
         QVAL = case_when(IDVARVAL == "1" | IDVARVAL == "5" ~ "4",
                          IDVARVAL == "2" | IDVARVAL == "6" ~ "5",
                          IDVARVAL == "3" | IDVARVAL == "7" ~ "4",
                          IDVARVAL == "4" | IDVARVAL == "8" ~ "8"),

         QVAL = as.character(QVAL),
         QORIG = "CRF"
  )

suppis <- suppis1


var_label(suppis$STUDYID) <- "Study Identifier"
var_label(suppis$RDOMAIN) <- "Related Domain Abbreviation"
var_label(suppis$USUBJID) <- "Unique Subject Identifier"
var_label(suppis$IDVAR) <- "Identifying Variable"
var_label(suppis$IDVARVAL) <- "Identifying Variable Value"
var_label(suppis$QNAM) <- "Qualifier Variable Name"
var_label(suppis$QLABEL) <- "Qualifier Variable Label"
var_label(suppis$QVAL) <- "Data Value"
var_label(suppis$QORIG) <- "Origin"
var_label(suppis$QEVAL) <- "Evaluator"


# Save RDA file
setwd("/shared-scratch/area/fb267539/Admiral")
str(suppis)
save('suppis',file = 'suppis.rda')
