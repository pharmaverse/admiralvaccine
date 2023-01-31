library(Hmisc)
library(dplyr)


table <- matrix(NA, nrow = 80, ncol = 27)

colnames(table) <- c("STUDYID","DOMAIN","USUBJID","ISSEQ","ISGRPID","ISREFID","ISSPID","ISLNKID",
                     "ISTESTCD","ISTEST","ISCAT","ISORRES","ISORRESU","ISSTRESC","ISSTRESN",
                     "ISSTRESU","ISSTAT","ISREASND","ISNAM","ISSPEC","ISMETHOD","ISBLFL","ISLLOQ",
                     "VISITNUM","EPOCH","ISDTC","ISDY")

table1 <- as.data.frame(table)

for (i in 1:NROW(table1)) {

  table1[i,1] <- as.character("999999")
  table1[i,2] <- as.character("IS")
  table1[i,5] <- as.character("XX")
  table1[i,11] <- as.character("IMMUNOLOGY")
  table1[i,13] <- as.character("1/DIL")
  table1[i,19] <- as.character("LABNAME")
  table1[i,20] <- as.character("SERUM")
  table1[i,21] <- as.character("METHODNAME")
}


table1$ISTESTCD <- rep(c("J0033VN","I0019NT","M0019LN","R0003MA"),20)


table1$USUBJID <- c(rep("999999-000001",8),rep("999999-000002",8),rep("999999-000003",8),rep("999999-000004",8),
                    rep("999999-000005",8),rep("999999-000006",8),rep("999999-000007",8),rep("999999-000008",8),
                    rep("999999-000009",8),rep("999999-000010",8))


table1$VISITNUM <- c(rep(10,4),rep(30,4),rep(10,4),rep(30,4),rep(10,4),rep(30,4),rep(10,4),rep(30,4),
                     rep(10,4),rep(30,4),rep(10,4),rep(30,4),rep(10,4),rep(30,4),rep(10,4),rep(30,4),
                     rep(10,4),rep(30,4),rep(10,4),rep(30,4))

set.seed(1234)
table1$ISORRES <- sample(c("2.0","3.0","5.0","<2",">8"), NROW(table1), replace = T)


is1 <- table1 %>%
  group_by(STUDYID,USUBJID) %>%
  mutate(ISSEQ = row_number()) %>%
  ungroup() %>%
  mutate(EPOCH = if_else(VISITNUM == 10, "FIRST TREATMENT","SECOND TREATMENT"),

         ISDTC = if_else(VISITNUM == 10, "1999-01-01","1999-03-01"),

         ISDY = if_else(VISITNUM == 10, "1","61"),

         ISBLFL = if_else(VISITNUM == 10, "Y",as.character(NA)),

         ISSTRESU = "titer",

         ISTEST = case_when(ISTESTCD == "J0033VN" ~ "J0033VN Antibody",
                            ISTESTCD == "I0019NT" ~ "I0019NT Antibody",
                            ISTESTCD == "M0019LN" ~ "M0019LN Antibody",
                            ISTESTCD == "R0003MA" ~ "R0003MA Antibody"),

         ISLLOQ = case_when(ISTESTCD == "J0033VN" ~ 2,
                            ISTESTCD == "I0019NT" ~ 4,
                            ISTESTCD == "M0019LN" ~ 8,
                            ISTESTCD == "R0003MA" ~ 4),

         ISSTRESC = case_when(ISORRES == "2.0" ~ "2",
                              ISORRES == "3.0" ~ "3",
                              ISORRES == "5.0" ~ "5",
                              ISORRES == "<2" ~ as.character(NA),
                              ISORRES == ">8" ~ as.character(NA)),

         ISSTRESN = case_when(ISORRES == "2.0" ~ 2,
                              ISORRES == "3.0" ~ 3,
                              ISORRES == "5.0" ~ 5,
                              ISORRES == "<2" ~ as.numeric(NA),
                              ISORRES == ">8" ~ as.numeric(NA))
         ) %>%
  group_by(STUDYID,USUBJID,VISITNUM) %>%
  mutate(ISREFID = 100000 + as.numeric(substr(USUBJID,8,14))) %>%
  ungroup() %>%
  mutate(LOD = case_when(ISTESTCD == "J0033VN" ~ 2,
                         ISTESTCD == "I0019NT" ~ 2,
                         ISTESTCD == "M0019LN" ~ 2,
                         ISTESTCD == "R0003MA" ~ 2))

# To add NOT DONE records
is <- is1 %>%
  mutate(ISORRES = if_else(row_number() == 1 | row_number() == 10, as.character(NA), ISORRES),
         ISSTRESC = if_else(row_number() == 1 | row_number() == 10, as.character(NA), ISSTRESC),
         ISSTRESN = if_else(row_number() == 1 | row_number() == 10, as.numeric(NA), ISSTRESN),
         ISSTRESU = if_else(row_number() == 1 | row_number() == 10, as.character(NA), ISSTRESU),
         ISSTAT = if_else(row_number() == 1 | row_number() == 10, "NOT DONE", as.character(NA)),
         ISREASND = if_else(row_number() == 1 | row_number() == 10, "NO SAMPLE TAKEN", as.character(NA))
         )


label(is$STUDYID) <- "Study Identifier"
label(is$DOMAIN) <- "Domain Abbreviation"
label(is$USUBJID) <- "Unique Subject Identifier"
label(is$ISSEQ) <- "Sequence Number"
label(is$ISGRPID) <- "Group ID"
label(is$ISREFID) <- "Reference ID"
label(is$ISSPID) <- "Sponsor-Defined Identifier"
label(is$ISLNKID) <- "Link ID"
label(is$ISTESTCD) <- "Immunogenicity Test/Exam Short Name"
label(is$ISTEST) <- "Immunogenicity Test or Examination Name"
label(is$ISCAT) <- "Category for Immunogenicity Test"
label(is$ISORRES) <- "Results or Findings in Original Units"
label(is$ISORRESU) <- "Original Units"
label(is$ISSTRESC) <- "Character Result/Finding in Std Format"
label(is$ISSTRESN) <- "Numeric Results/Findings in Std. Units"
label(is$ISSTRESU) <- "Standard Units"
label(is$ISSTAT) <- "Completion Status"
label(is$ISREASND) <- "Reason Not Done"
label(is$ISNAM) <- "Vendor Name"
label(is$ISSPEC) <- "Specimen Type"
label(is$ISMETHOD) <- "Method of Test or Examination"
label(is$ISBLFL) <- "Baseline Flag"
label(is$ISLLOQ) <- "Lower Limit of Quantitation"
label(is$VISITNUM) <- "Visit Number"
label(is$EPOCH) <- "Epoch"
label(is$ISDTC) <- "Date/Time of Collection"
label(is$ISDY) <- "Study Day of Visit/Collection/Exam"
label(is$LOD) <- "Limit of Detection"


is <- is %>%
  select(-c(ISGRPID,ISREFID,ISSPID,ISLNKID))


# Save RDA file
setwd("C:/")
str(is)
save('is',file = 'is.rda')




setwd("C:/")
write.csv(is, "is.csv", row.names = F, col.names = T)

