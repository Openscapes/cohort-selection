library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

source("R/functions.R")

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "1fpMJpReMuAW3W_U7rGSBkM1BgzWdVySLqLec-xoZYUc"

ss_edited <- read_sheet(signup_sheet, sheet = "cohort-picks", range = "A1:R116")

cohort_summary <- ss_edited |>
  rename(cohort = cohort_pick) |>
  count(cohort) |>
  arrange(cohort)
View(cohort_summary)

parent_division_summary <- ss_edited |>
  count(parent_division) |>
  arrange(desc(n))
View(parent_division_summary)

division_summary <- ss_edited |>
  count(parent_division, division) |>
  arrange(desc(n))
View(division_summary)

team_summary <- ss_edited |>
  mutate(
    team_name = ifelse(is.na(team_name), "No team", team_name)
  ) |>
  count(team_name) |>
  arrange(desc(n))
View(team_summary)

team_by_division_summary <- ss_edited |>
  count(team_name, parent_division) |>
  arrange(team_name, parent_division)

View(team_by_division_summary)

ss_edited |>
  count(parent_division, cohort_pick) |>
  View()

sheet_delete(ss = signup_sheet, sheet = "post-edit-summaries")
sheet_add(ss = signup_sheet, sheet = "post-edit-summaries")

range_write(
  ss = signup_sheet,
  data = cohort_summary,
  sheet = "post-edit-summaries",
  range = "A1"
)

range_write(
  ss = signup_sheet,
  data = parent_division_summary,
  sheet = "post-edit-summaries",
  range = "D1"
)

range_write(
  ss = signup_sheet,
  data = division_summary,
  sheet = "post-edit-summaries",
  range = "G1"
)

range_write(
  ss = signup_sheet,
  data = team_summary,
  sheet = "post-edit-summaries",
  range = "K1"
)

range_write(
  ss = signup_sheet,
  data = team_by_division_summary,
  sheet = "post-edit-summaries",
  range = "N1"
)

## Create groups for seaside chats
ss_edited |>
  group_by(team_name) |>
  summarise(members = paste(first_name, last_name, collapse = "; ")) |>
  View()
