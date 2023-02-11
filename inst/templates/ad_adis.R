# Name: ADIS
#
# Label: Immunogenicity Analysis
#
# Input: xx, xx, xx
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
install.packages("metatools", repos = "https://cloud.r-project.org")
library(metatools)

# Add your template ADaM script code

# STEP 1 - combine IS with SUPPIS.

# Please, upload IS and SUPPIS from MOCK DATA folder.
is_suppis <- combine_supp(is, suppis)

adis <- is_suppis %>%
  mutate(
    AVISITN = as.numeric(VISITNUM),
    AVISIT = case_when(
      VISITNUM == 10 ~ "Visit 1",
      VISITNUM == 20 ~ "Visit 2",
      VISITNUM == 30 ~ "Visit 3",
      VISITNUM == 40 ~ "Visit 4",
      is.na(VISITNUM) ~ NA_character_
    ),
    ATPTN = as.numeric(VISITNUM / 10),
    ATPT = case_when(
      VISITNUM == 10 ~ "Visit 1 (Day 1)",
      VISITNUM == 20 ~ "Visit 2 (Day #)",
      VISITNUM == 30 ~ "Visit 3 (Day #)",
      VISITNUM == 40 ~ "Visit 4 (Day #)",
      is.na(VISITNUM) ~ NA_character_
    ),
    ATPTREF = case_when(
      VISITNUM %in% c(10, 20) ~ "FIRST TREATMENT",
      VISITNUM %in% c(30, 40) ~ "SECOND TREATMENT",
      is.na(VISITNUM) ~ NA_character_
    )
  )
