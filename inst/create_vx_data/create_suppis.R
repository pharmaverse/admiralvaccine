table_supp <- matrix(NA, nrow = 16, ncol = 10)

colnames(table_supp) <- c(
  "STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
  "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL"
)

table_supp1 <- as.data.frame(table_supp)

suppis1 <- table_supp1 %>%
  mutate(USUBJID = case_when(
    row_number() >= 1 & row_number() <= 8 ~ "ABC-1001",
    row_number() >= 9 & row_number() <= 16 ~ "ABC-1002"
  )) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(IDVARVAL = row_number()) %>%
  ungroup() %>%
  mutate(
    STUDYID = "ABC",
    RDOMAIN = "IS",
    IDVAR = "ISSEQ",
    QNAM = "LOD",
    QLABEL = "Limit of Detection",
    IDVARVAL = as.character(IDVARVAL),
    QVAL = case_when(
      IDVARVAL == "1" | IDVARVAL == "5" ~ "4",
      IDVARVAL == "2" | IDVARVAL == "6" ~ "5",
      IDVARVAL == "3" | IDVARVAL == "7" ~ "4",
      IDVARVAL == "4" | IDVARVAL == "8" ~ "8"
    ),
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

vx_suppis <- suppis
