library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

source("R/functions.R")

set.seed(6)

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "1fpMJpReMuAW3W_U7rGSBkM1BgzWdVySLqLec-xoZYUc"
os_mainlist <- "10ub0NKrPa1phUa_X-Jxg8KYH57WGLaZzBN-vQT4e10o"

ss_raw <- read_signup_sheet(signup_sheet)

team_lookup <- read_sheet(signup_sheet, sheet = "Notes")

prev_participants <- read_sheet(os_mainlist) |>
  mutate(email = tolower(email))

# Join signup sheet answers to the mentors copy which has annotations we
# need (supervisor, new hire, si? (though we can get si from columns in raw sheet
# as well))
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
  left_join(team_lookup, by = "team_name")

# Find those who didn't choose yes for any cohort
ss |>
  filter(
    !grepl("yes", cohort_a),
    !grepl("yes", cohort_b),
    !grepl("yes", cohort_c)
  ) |>
  pull(email_address) |>
  cat(sep = ", ")

# Get proportions of yes/no/probably by team
cohort_prefs <- ss |>
  group_by(simple_team_name) |>
  summarise(
    n = n(),
    prop_a = mean(cohort_a == "yes", na.rm = TRUE),
    prop_b = mean(cohort_b == "yes", na.rm = TRUE),
    prop_c = mean(cohort_c == "yes", na.rm = TRUE),
    prop_a_probably = mean(cohort_a != "no", na.rm = TRUE),
    prop_b_probably = mean(cohort_b != "no", na.rm = TRUE),
    prop_c_probably = mean(cohort_c != "no", na.rm = TRUE)
  )

team_cohort_prefs <- cohort_prefs |>
  filter(!is.na(simple_team_name))

team_cohort_prefs |> arrange(desc(n)) |> View()

## Assign cohorts for teams first, using the highest proportion of "yes" responses,
## and then if there's a tie, randomly select one of those tied cohorts.
team_cohort_selections <- team_cohort_prefs |>
  select(!ends_with("probably")) |>
  pivot_longer(
    cols = matches("prop_[a-c]$"),
    names_to = "cohort",
    values_to = "proportion"
  ) |>
  mutate(cohort = stringr::str_replace(cohort, "prop_", "")) |>
  filter(proportion > 0) |>
  group_by(simple_team_name) |>
  # Get the cohort with the highest proportion of "yes" responses
  slice_max(order_by = proportion, n = 1, with_ties = TRUE) |>
  slice_sample(n = 1)

team_cohort_selections |>
  group_by(cohort) |>
  summarise(n = sum(n))

## Now assign cohorts for individuals not on a team, using a weighted random sample.
## The weights really rough, just reflecting that A is very full from teams
individual_cohort_selections <- ss |>
  filter(is.na(team_name)) |>
  select(email_address, simple_team_name, cohort_a, cohort_b, cohort_c) |>
  pivot_longer(
    cols = starts_with("cohort_"),
    names_to = "cohort",
    values_to = "response"
  ) |>
  mutate(cohort = stringr::str_replace(cohort, "cohort_", "")) |>
  filter(response == "yes") |>
  mutate(
    weights = case_when(
      cohort == "b" ~ 0.9,
      cohort == "c" ~ 0.5,
      TRUE ~ 0.01
    )
  ) |>
  group_by(email_address) |>
  # Get the cohort with the highest proportion of "yes" responses
  slice_sample(n = 1, weight_by = weights)

individual_cohort_selections |>
  ungroup() |>
  count(cohort)

## Assign indiviuals to their cohorts
final_cohorts <- ss |>
  left_join(
    team_cohort_selections |>
      select(simple_team_name, team_cohort_pick = cohort),
    by = "simple_team_name"
  ) |>
  left_join(
    individual_cohort_selections |>
      select(email_address, individual_cohort_pick = cohort),
    by = "email_address"
  ) |>
  mutate(
    cohort_pick = case_when(
      is.na(team_cohort_pick) ~ individual_cohort_pick,
      .default = team_cohort_pick
    )
  ) |>
  ungroup()

## Summarise cohorts to check numbers before writing back to sheet
cohort_summary <- final_cohorts |>
  rename(cohort = cohort_pick) |>
  count(cohort) |>
  arrange(cohort)
View(cohort_summary)

## Write to sheet
write_sheet(final_cohorts, ss = signup_sheet, sheet = "cohort-picks")

## The rest of the summaries
parent_division_summary <- final_cohorts |>
  count(parent_division) |>
  arrange(desc(n))
View(parent_division_summary)

division_summary <- final_cohorts |>
  count(parent_division, division) |>
  arrange(desc(n))
View(division_summary)

team_summary <- final_cohorts |>
  mutate(
    team_name = ifelse(is.na(simple_team_name), "No team", simple_team_name)
  ) |>
  count(team_name) |>
  arrange(desc(n))
View(team_summary)

team_by_division_summary <- final_cohorts |>
  filter(!is.na(simple_team_name)) |>
  count(simple_team_name, parent_division) |>
  rename(team_name = simple_team_name) |>
  arrange(team_name, parent_division)

View(team_by_division_summary)


## Add summaries to selection spreadsheet.
sheet_delete(ss = signup_sheet, sheet = "summaries")
sheet_add(ss = signup_sheet, sheet = "summaries")

range_write(
  ss = signup_sheet,
  data = cohort_summary,
  sheet = "summaries",
  range = "A1"
)

range_write(
  ss = signup_sheet,
  data = parent_division_summary,
  sheet = "summaries",
  range = "D1"
)

range_write(
  ss = signup_sheet,
  data = division_summary,
  sheet = "summaries",
  range = "G1"
)

range_write(
  ss = signup_sheet,
  data = team_summary,
  sheet = "summaries",
  range = "K1"
)

range_write(
  ss = signup_sheet,
  data = team_by_division_summary,
  sheet = "summaries",
  range = "N1"
)
