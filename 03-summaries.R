source("R/header.R")
source("R/functions.R")

current_cohort_selection <- read_sheet(signup_sheet_mentor_cp, final_worksheet)

accepted_summary <- pivot_summary(current_cohort_selection, status)
division_summary <- pivot_summary(current_cohort_selection, division)
si_name_summary <- pivot_summary(current_cohort_selection, si_name)
team_name_summary <- pivot_summary(current_cohort_selection, team_name)

sheet_add(signup_sheet_mentor_cp, sheet = summary_sheet)

range_write(
  signup_sheet_mentor_cp,
  accepted_summary,
  sheet = summary_sheet,
  range = "A1"
)

range_write(
  signup_sheet_mentor_cp,
  division_summary,
  sheet = summary_sheet,
  range = "H1"
)

range_write(
  signup_sheet_mentor_cp,
  si_name_summary,
  sheet = summary_sheet,
  range = "O1"
)

range_write(
  signup_sheet_mentor_cp,
  team_name_summary,
  sheet = summary_sheet,
  range = "V1"
)