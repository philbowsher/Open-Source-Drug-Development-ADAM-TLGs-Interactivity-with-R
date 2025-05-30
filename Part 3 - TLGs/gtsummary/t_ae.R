library(gtsummary)
library(dplyr)

tbl_hierarchical(
  data = adae,
  variables = c(AEBODSYS, AEDECOD),
  by = ACTARM,
  id = USUBJID,
  denominator = adsl,
  include = AEDECOD,
  overall_row = TRUE,
  label = list(
    AEBODSYS = "Body System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    ..ard_hierarchical_overall.. = "Total number of patients with at least one adverse event")
)
