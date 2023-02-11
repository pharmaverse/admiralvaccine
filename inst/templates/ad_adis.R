# Name: ADIS
#
# Label: Immunogenicity Analysis
#
# Input: xx, xx, xx
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
install.packages("metatools")
library(metatools)

# Add your template ADaM script code

# STEP 1 - combine IS with SUPPIS.

# Please, upload IS and SUPPIS from MOCK DATA folder.
is_suppis <- combine_supp(is, suppis)
