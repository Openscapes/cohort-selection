library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "10zTz6i-ZuwnQhCTG63jRku0BtVwgc9e1tqnfGuiyM2c"
signup_sheet_mentor_cp <- "1z8VWGExea-0X2olPVJINLXgimcweWIUMAuI7mU6UYKg"

os_mainlist <- "10ub0NKrPa1phUa_X-Jxg8KYH57WGLaZzBN-vQT4e10o"

# Worksheet for annotation by mentors
annotated_ws <- "annotated-signups_2024-09-23"

# Final worksheet for fine-tuning selection
final_worksheet <- "final-selection_2024-09-25"

# Summary worksheet
summary_sheet <- paste0("summary-", final_worksheet)