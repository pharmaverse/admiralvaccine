table_supp <- matrix(NA, nrow = 80, ncol = 8)

colnames(table_supp) <- c("STUDYID","RDOMAIN","USUBJID","IDVAR","IDVARVAL","QNAM","QLABEL","QVAL")

table_supp1 <- as.data.frame(table_supp)


table_supp1$USUBJID <- c(rep("999999-000001",8),rep("999999-000002",8),rep("999999-000003",8),rep("999999-000004",8),
                    rep("999999-000005",8),rep("999999-000006",8),rep("999999-000007",8),rep("999999-000008",8),
                    rep("999999-000009",8),rep("999999-000010",8))


suppis1 <- table_supp1 %>%
  group_by(STUDYID,USUBJID) %>%
  mutate(IDVARVAL = row_number()) %>%
  ungroup() %>%
  mutate(STUDYID = 999999,
         RDOMAIN = "IS",
         IDVAR = "ISSEQ",
         QNAM = "LOD",
         QLABEL = "Limit of Detection",
         IDVARVAL = as.character(IDVARVAL),
         QVAL = case_when(IDVARVAL == "1" | IDVARVAL == "5" ~ "4",
                          IDVARVAL == "2" | IDVARVAL == "6" ~ "5",
                          IDVARVAL == "3" | IDVARVAL == "7" ~ "4",
                          IDVARVAL == "4" | IDVARVAL == "8" ~ "8"),
         
         QVAL = as.character(QVAL)
         )

suppis <- suppis1


label(suppis$STUDYID) <- "Study Identifier"
label(suppis$RDOMAIN) <- "Related Domain Abbreviation"
label(suppis$USUBJID) <- "Unique Subject Identifier"
label(suppis$IDVAR) <- "Identifying Variable"
label(suppis$IDVARVAL) <- "Identifying Variable Value"
label(suppis$QNAM) <- "Qualifier Variable Name"
label(suppis$QLABEL) <- "Qualifier Variable Label"
label(suppis$QVAL) <- "Data Value"


# Save RDA file
setwd("C:/")
str(suppis)
save('suppis',file = 'suppis.rda')
