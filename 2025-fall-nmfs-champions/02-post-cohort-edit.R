library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

source("R/functions.R")

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "1fpMJpReMuAW3W_U7rGSBkM1BgzWdVySLqLec-xoZYUc"
os_mainlist <- "10ub0NKrPa1phUa_X-Jxg8KYH57WGLaZzBN-vQT4e10o"

ss_raw <- read_signup_sheet(signup_sheet)

team_lookup <- read_sheet(signup_sheet, sheet = "Notes")

prev_participants <- read_sheet(os_mainlist) |>
  mutate(email = tolower(email))

ss_edited <- read_sheet(signup_sheet, sheet = "cohort-picks", range = "A1:R113")

ss <- ss_raw |>
  # Join to previous participants sheet so we can flag them
  left_join(
    prev_participants |>
      select(email, cohort) |>
      group_by(email) |>
      summarise(prev_cohort = paste(cohort, collapse = " / ")),
    by = c("email_address" = "email"),
    na_matches = "never"
  ) |>
  mutate(
    prev_champion = !is.na(prev_cohort)
  ) |>
  mutate(
    division = stringr::str_replace(division, "\\s*\\(.*\\)", ""),
    parent_division = stringr::str_split_i(division, "[- /]", 1)
  ) |>
  left_join(team_lookup, by = "team_name") |>
  mutate(across(starts_with("cohort_"), \(x) {
    ifelse(grepl("yes", x), "yes", x)
  }))

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

ss |>
  left_join(
    ss_edited |>
      select(email_address, team_name, cohort_pick),
    by = "email_address"
  ) |>
  filter(team_name.x != team_name.y) |>
  View()

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
  range = "N1"
)

## Create groups for seaside chats
ss_edited |>
  group_by(team_name) |>
  summarise(members = paste(first_name, last_name, collapse = "; ")) |>
  View()
