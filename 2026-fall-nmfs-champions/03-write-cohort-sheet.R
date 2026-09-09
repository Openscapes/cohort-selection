library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

source("R/functions.R")

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "1hLmrrvajZb9yR8Vt8ngUPp4wdCeBeYjFBxb02OvX4nU"
cohorts_sheet <- "1C32ykwz1wJXMHN5RH_azqNL6jQvpSBN-44USgdSszMI"

## Final, reviewed assignments live in the `cohort-picks` tab of the
## signup workbook (written by 01, manually edited, validated by 02)
picks <- read_sheet(signup_sheet, sheet = "cohort-picks")

## Map picks onto the ParticipantsList roster layout. Every target tab
## already has a header row, so all writes below use col_names = FALSE:
##   - cohort tabs: headers at row 6, data from row 7 (col I is the
##     "*github_username" column participants fill in themselves)
##   - 2026-all: headers at row 1, data from row 2
##   - SeasideChatFormations: headers at row 7, data from row 8
roster <- picks |>
  transmute(
    cohort = paste0("2026-nmfs-champions-", cohort_pick),
    first = first_name,
    last = last_name,
    division,
    email = email_address,
    team_name = simple_team_name,
    briefly_describe,
    team_needs
  )

## Write each cohort to its own tab
for (tab in c("a", "b", "c")) {
  range_write(
    ss = cohorts_sheet,
    data = filter(roster, cohort == paste0("2026-nmfs-champions-", tab)),
    sheet = paste0("2026-nmfs-champions-", tab),
    range = "A7",
    col_names = FALSE
  )
}

## `2026-all` holds the union of the three cohorts
range_write(
  ss = cohorts_sheet,
  data = arrange(roster, cohort, team_name),
  sheet = "2026-all",
  range = "A2",
  col_names = FALSE
)

## Seaside Chats: pre-fill one row per team with the group name (col A)
## and its members (col E); description/point person/scheduling columns
## are for the teams to complete themselves. Only teams with more than
## one member are included — a "team" of one is not a peer group. Note
## Agent-Coders spans cohorts A and B (the one intentional team split).
seaside_teams <- picks |>
  filter(!is.na(simple_team_name)) |>
  summarise(
    n_members = n(),
    members = paste(first_name, last_name, collapse = "; "),
    .by = simple_team_name
  ) |>
  filter(n_members > 1) |>
  select(-n_members) |>
  arrange(simple_team_name)

range_write(
  ss = cohorts_sheet,
  data = select(seaside_teams, team_name = simple_team_name),
  sheet = "SeasideChatFormations",
  range = "A8",
  col_names = FALSE
)
range_write(
  ss = cohorts_sheet,
  data = select(seaside_teams, members),
  sheet = "SeasideChatFormations",
  range = "E8",
  col_names = FALSE
)
