library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

source("R/functions.R")

set.seed(4187)

gs4_auth(email = "andy@openscapes.org")

signup_sheet <- "1hLmrrvajZb9yR8Vt8ngUPp4wdCeBeYjFBxb02OvX4nU"
os_mainlist <- "10ub0NKrPa1phUa_X-Jxg8KYH57WGLaZzBN-vQT4e10o"

ss_raw <- read_signup_sheet(signup_sheet, sheet = "Fall 2026 Champions") |>
  filter(
    !is.na(email_address),
    email_address != "tbd",
    # Drops the "example: Jonathan Peake" instructional row
    !str_starts(tolower(first_name), "example")
  )

prev_participants <- read_sheet(os_mainlist) |>
  mutate(email = tolower(email))

ss <- ss_raw |>
  # Join to previous participants sheet so we can flag returning champions
  left_join(
    prev_participants |>
      select(email, cohort) |>
      group_by(email) |>
      summarise(prev_cohort = paste(cohort, collapse = " / ")),
    by = c("email_address" = "email"),
    na_matches = "never"
  ) |>
  mutate(prev_champion = !is.na(prev_cohort)) |>
  mutate(
    division_raw = division,
    division = str_squish(division),
    division = str_replace_all(division, "_", "/"),
    division = str_replace_all(division, "\\s*/\\s*", "/"),
    # "NMFS" is the umbrella agency so drop it and use the next segment
    division = str_replace(division, regex("^NMFS\\s*/?\\s*"), ""),
    division = na_if(division, ""),
    parent_division = str_split_i(division, "[-/ ]", 1),
    # A few respondents named two offices at once (e.g. "NMFS OPR and WCR",
    # "NWFSC/SWFSC", "OCED/AOML also on projects with SEFSC"). Take the
    # first office listed as `parent_division` and flag for review.
    division_multi_office = str_detect(
      division_raw,
      regex("\\band\\b|also on", ignore_case = TRUE)
    )
  ) |>
  mutate(
    # Treat "none" / "individual" / "TBD" / "N/A" team names as no team
    simple_team_name = str_squish(team_name),
    simple_team_name = if_else(
      tolower(simple_team_name) %in%
        c("none", "na", "n/a", "individual", "tbd"),
      NA_character_,
      simple_team_name
    )
  ) |>
  # The sheet uses the literal text "null" where a cohort checkbox wasn't
  # answered - replace with NA
  mutate(across(starts_with("cohort_"), \(x) na_if(x, "null")))

## ---- Team-level cohort assignment -----------------------------------
## Goal: keep teams together as much as possible, choosing whichever cohort
## the team has the strongest combined preference for. Score = proportion
## saying "yes" + 0.5 * proportion saying "unsure" (excluding non-responses),
## so a team with only "unsure" answers still gets compared
## across cohorts rather than being treated as having no preference.
team_cohort_prefs <- ss |>
  filter(!is.na(simple_team_name)) |>
  group_by(simple_team_name) |>
  summarise(
    n = n(),
    yes_a = mean(cohort_a == "yes", na.rm = TRUE),
    yes_b = mean(cohort_b == "yes", na.rm = TRUE),
    yes_c = mean(cohort_c == "yes", na.rm = TRUE),
    unsure_a = mean(cohort_a == "unsure", na.rm = TRUE),
    unsure_b = mean(cohort_b == "unsure", na.rm = TRUE),
    unsure_c = mean(cohort_c == "unsure", na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), \(x) ifelse(is.nan(x), NA, x)))

team_cohort_scores <- team_cohort_prefs |>
  pivot_longer(
    cols = matches("^(yes|unsure)_[abc]$"),
    names_to = c("stat", "cohort"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(
    score = coalesce(yes, 0) + 0.5 * coalesce(unsure, 0),
    has_info = !is.na(yes) | !is.na(unsure)
  )

## Assign cohorts for teams first, using the highest score, and if there's a
## tie, randomly select one of the tied cohorts.
team_cohort_selections <- team_cohort_scores |>
  mutate(has_any_team_info = any(has_info), .by = simple_team_name) |>
  group_by(simple_team_name) |>
  slice_max(order_by = score, n = 1, with_ties = TRUE) |>
  slice_sample(n = 1) |>
  ungroup() |>
  select(
    simple_team_name,
    team_cohort_pick = cohort,
    team_score = score
  )

team_cohort_selections |>
  count(team_cohort_pick)

## ---- Individual (no-team) cohort assignment --------------------------
## Individuals are assigned by checking their "yes" cohorts first: among
## those, prefer whichever currently has the most room relative to an even
## three-way split of the roster. Only if none of their "yes" cohorts have
## room left do we consider cohorts they were "unsure" about, using the same
## room-based rule. This guarantees nobody is routed into a cohort they were
## merely "unsure" about while a cohort they actually said "yes" to still
## has space -- room is checked before preference strength is downgraded.
team_size_by_cohort <- ss |>
  filter(!is.na(simple_team_name)) |>
  count(simple_team_name, name = "team_n") |>
  left_join(team_cohort_selections, by = "simple_team_name") |>
  summarise(n_people = sum(team_n), .by = team_cohort_pick)

target_per_cohort <- nrow(ss) / 3

cohort_counts <- c(a = 0, b = 0, c = 0)
cohort_counts[
  team_size_by_cohort$team_cohort_pick
] <- team_size_by_cohort$n_people

individuals_long <- ss |>
  filter(is.na(simple_team_name)) |>
  select(email_address, cohort_a, cohort_b, cohort_c) |>
  pivot_longer(
    cols = starts_with("cohort_"),
    names_to = "cohort",
    names_prefix = "cohort_",
    values_to = "response"
  )

# From a candidate set of cohorts, pick whichever has the most room against
# target_per_cohort (ties/weighting by remaining room); if none have room
# left, fall back to whichever is least over, so someone still gets placed
pick_roomiest <- function(cohorts, target_per_cohort) {
  if (length(cohorts) == 0) {
    return(NA_character_)
  }
  room <- target_per_cohort - cohort_counts[cohorts]
  with_room <- cohorts[room > 0]
  if (length(with_room) > 0) {
    sample(with_room, 1, prob = room[room > 0])
  } else {
    cohorts[which.max(room)]
  }
}

individual_cohort_selections <- tibble(
  email_address = character(),
  individual_cohort_pick = character()
)

# Process people in random order so fill order isn't driven by row order
# in the sheet; cohort_counts updates after each placement so later people
# see the effect of earlier placements
for (person in sample(unique(individuals_long$email_address))) {
  resp <- individuals_long |> filter(email_address == person)
  yes_cohorts <- resp |> filter(response == "yes") |> pull(cohort)
  unsure_cohorts <- resp |> filter(response == "unsure") |> pull(cohort)

  pick <- pick_roomiest(yes_cohorts, target_per_cohort)
  if (is.na(pick)) {
    pick <- pick_roomiest(unsure_cohorts, target_per_cohort)
  }

  # Anyone with no "yes"/"unsure" anywhere (said "no"/didn't answer for all
  # three) is left unassigned here and handled by the fallback below
  if (!is.na(pick)) {
    cohort_counts[pick] <- cohort_counts[pick] + 1
    individual_cohort_selections <- bind_rows(
      individual_cohort_selections,
      tibble(email_address = person, individual_cohort_pick = pick)
    )
  }
}

individual_cohort_selections |>
  count(individual_cohort_pick)

## ---- Combine team + individual picks, with a last-resort fallback ----
# Remaining room per cohort after all team and individual placements above,
# used only for the last-resort fallback below
remaining_capacity <- tibble(
  cohort = names(cohort_counts),
  deficit = pmax(target_per_cohort - cohort_counts, 1)
)

final_cohorts <- ss |>
  left_join(
    team_cohort_selections,
    by = "simple_team_name"
  ) |>
  left_join(individual_cohort_selections, by = "email_address") |>
  mutate(
    no_cohort_preference = is.na(team_cohort_pick) &
      is.na(individual_cohort_pick),
    # Last resort: someone with no team and no usable preference at all
    # (e.g. said "no" to everything, or never answered). Assign purely by
    # remaining capacity and flag for manual review.
    fallback_pick = if_else(
      no_cohort_preference,
      sample(
        remaining_capacity$cohort,
        n(),
        replace = TRUE,
        prob = remaining_capacity$deficit
      ),
      NA_character_
    ),
    cohort_pick = coalesce(
      team_cohort_pick,
      individual_cohort_pick,
      fallback_pick
    )
  ) |>
  mutate(
    said_no_to_assigned_cohort = case_when(
      cohort_pick == "a" ~ coalesce(cohort_a == "no", FALSE),
      cohort_pick == "b" ~ coalesce(cohort_b == "no", FALSE),
      cohort_pick == "c" ~ coalesce(cohort_c == "no", FALSE),
      .default = FALSE
    ),
    # Rows worth a human look before this goes final: team pick was a coin
    # flip, person had no usable signal at all, division named two offices,
    # or a team's pick landed on a cohort this person personally said "no" to
    needs_review = no_cohort_preference |
      said_no_to_assigned_cohort
  ) |>
  ungroup()

## Summarise cohorts to check numbers before writing back to sheet
cohort_summary <- final_cohorts |>
  count(cohort_pick) |>
  arrange(cohort_pick)
View(cohort_summary)

review_summary <- final_cohorts |>
  filter(needs_review) |>
  select(
    first_name,
    last_name,
    simple_team_name,
    division_raw,
    cohort_pick,
    cohort_a,
    cohort_b,
    cohort_c,
    no_cohort_preference,
    division_multi_office,
    said_no_to_assigned_cohort
  )
View(review_summary)

## Quality check: no team should be split across cohorts
team_split_check <- final_cohorts |>
  filter(!is.na(simple_team_name)) |>
  distinct(simple_team_name, cohort_pick) |>
  count(simple_team_name) |>
  filter(n > 1)
team_split_check

## ---- The rest of the summaries ----------------------------------------
parent_division_summary <- final_cohorts |>
  count(parent_division) |>
  arrange(desc(n))
View(parent_division_summary)

division_summary <- final_cohorts |>
  count(parent_division, division) |>
  arrange(desc(n))
View(division_summary)

team_summary <- final_cohorts |>
  mutate(team_name = coalesce(simple_team_name, "No team")) |>
  count(team_name) |>
  arrange(desc(n))
View(team_summary)

team_by_division_summary <- final_cohorts |>
  filter(!is.na(simple_team_name)) |>
  count(simple_team_name, parent_division) |>
  rename(team_name = simple_team_name) |>
  arrange(team_name, parent_division)
View(team_by_division_summary)

parent_division_by_cohort <- final_cohorts |>
  count(parent_division, cohort_pick) |>
  pivot_wider(names_from = cohort_pick, values_from = n, values_fill = 0)
View(parent_division_by_cohort)

## Uncomment once the assignments above have been reviewed.
# write_sheet(final_cohorts, ss = signup_sheet, sheet = "cohort-picks")
#
# sheet_delete(ss = signup_sheet, sheet = "summaries")
# sheet_add(ss = signup_sheet, sheet = "summaries")
#
# range_write(ss = signup_sheet, data = cohort_summary, sheet = "summaries", range = "A1")
# range_write(ss = signup_sheet, data = parent_division_summary, sheet = "summaries", range = "D1")
# range_write(ss = signup_sheet, data = division_summary, sheet = "summaries", range = "G1")
# range_write(ss = signup_sheet, data = team_summary, sheet = "summaries", range = "K1")
# range_write(ss = signup_sheet, data = team_by_division_summary, sheet = "summaries", range = "N1")
# range_write(ss = signup_sheet, data = parent_division_by_cohort, sheet = "summaries", range = "R1")
