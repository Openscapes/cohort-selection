library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

source("R/functions.R")

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "1hLmrrvajZb9yR8Vt8ngUPp4wdCeBeYjFBxb02OvX4nU"

## Read back the `cohort-picks` sheet after it has been reviewed/edited
ss_edited <- read_sheet(signup_sheet, sheet = "cohort-picks")

cohort_summary <- ss_edited |>
  count(cohort_pick) |>
  arrange(cohort_pick)
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
  mutate(team_name = coalesce(simple_team_name, "No team")) |>
  group_by(team_name, cohort = cohort_pick) |>
  summarise(
    n = n()
  ) |>
  arrange(desc(n))
View(team_summary)

team_by_division_summary <- ss_edited |>
  filter(!is.na(simple_team_name)) |>
  count(simple_team_name, parent_division) |>
  arrange(simple_team_name, parent_division)
View(team_by_division_summary)

## Check whether any team ended up split across cohorts after manual edits
team_split_check <- ss_edited |>
  filter(!is.na(simple_team_name)) |>
  distinct(simple_team_name, cohort_pick) |>
  count(simple_team_name) |>
  filter(n > 1)
View(team_split_check)

# sheet_delete(ss = signup_sheet, sheet = "post-edit-summaries")
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
  range = "O1"
)
