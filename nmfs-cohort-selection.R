library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

gs4_auth(email = "andy@openscapes.org")

signup_sheet_mentor_cp <- "1z8VWGExea-0X2olPVJINLXgimcweWIUMAuI7mU6UYKg"

is_true_v <- Vectorize(isTRUE)

worksheet <- paste0("annotated-signups_2024-09-23")

## Read back after further annotation by mentors
ss_new <- read_sheet(
  signup_sheet_mentor_cp,
  sheet = worksheet,
  skip = 3 # Verify this in the sheet
)

small_divs <- ss_new |>
  count(division) |>
  filter(n <= 5) |>
  pull(division) |>
  na.omit()

ss_new <- ss_new |>
  # Add new derived columns
  mutate(
    # Treat NA in cohort column as "no"
    across(starts_with("cohort_"), \(x) ifelse(is.na(x), "no", x)),
    new_office = grepl("(OPR)|(OST)|(AKRO)|(GARFO)", division),
    points = is_true_v(new_hire) + is_true_v(si) + is_true_v(supervisor) + is_true_v(new_office),
    none_avail = cohort_a == "no" & cohort_b == "no" & cohort_c == "no",
    priority = ((!prev_champion & !is_true_v(mentor) & points > 0) | is_true_v(override_yes)) &
      !is_true_v(none_avail) ,
    ## fewer than 5 in a division, accept all
    priority = priority | (!is_true_v(none_avail) & division %in% small_divs),
    cohort_b = ifelse(is_true_v(division == "PIFSC"), "no", cohort_b)
  )

# For reproducible selection

# for (i in 1:20) {
i <- 20
set.seed(i)

ss_picked <- ss_new |>
  pivot_longer(
    cols = c(cohort_a, cohort_b, cohort_c),
    names_to = "cohort",
    names_transform = \(x) toupper(gsub("cohort_", "", x)),
    values_to = "cohort_available"
  ) |>
  mutate(
    cohort_weight = case_when(
      cohort_available == "yes" ~ 3,
      cohort_available == "if necessary" ~ 2,
      grepl("prefer other", cohort_available, ignore.case = TRUE) ~ 2,
      grepl("okay", cohort_available, ignore.case = TRUE) ~ 2,
      grepl("yes.+but", cohort_available, ignore.case = TRUE) ~ 1,
      grepl("cannot make", cohort_available, ignore.case = TRUE) ~ 1,
      grepl("except", cohort_available, ignore.case = TRUE) ~ 1,
      grepl("rather not", cohort_available, ignore.case = TRUE) ~ 1,
      cohort_available == "unsure" ~ 1,
      .default = 0
    )
  ) |>
  group_by(priority, email_address) |>
  # Next assign priority with > 1 cohort choice, choose "most available cohort"
  # based on cohort weight. This should have assigned all "priority" to a
  # cohort
  mutate(
    rand = rnorm(n()),
    pick = cohort_weight > 0 & cohort_weight == max(cohort_weight) &
      rand == max(rand[cohort_weight == max(cohort_weight)]) # break ties when equal weights (i.e., 2 or 3 "yes"s)
  )

cohort_assigned <- ss_picked |>
  ungroup() |>
  filter(pick) |>
  select(-(cohort_available:pick)) |>
  arrange(cohort, desc(priority), desc(points))

# Did we get everyone?
setdiff(ss_new$email_address, cohort_assigned$email_address)
setdiff(cohort_assigned$email_address, ss_new$email_address)

print(i)
cohort_assigned |>
  count(priority, cohort) |>
  print()
# }

## Fix allocation of teams:
##   - no more than 18 across cohorts

## Big divisions
max_total <- 18

## Make a data.frame of the maximum number of non-priority
## applicants that can be accepted, across cohorts
big_divs <- cohort_assigned |>
  count(division, priority) |>
  filter(sum(n) > max_total, .by = "division") |>
  filter(priority) |>
  summarise(
    max_extra_accepted = max_total - n,
    .by = "division"
  )

# Initial selection without enforcing quotas
cohort_selection_init <- cohort_assigned |>
  mutate(
    number = seq_len(n()),
    accepted = number <= 40,
    .by = "cohort"
  )

cohort_selection_init |>
  filter(accepted) |>
  count(division, name = "n_selected")

# Find those overrepresented
over_reps <- cohort_selection_init |>
  filter(accepted) |>
  count(division, name = "n_selected") |>
  filter(n_selected > max_total) |>
  left_join(big_divs, by = "division") |>
  left_join(cohort_selection_init, by = "division") |>
  filter(accepted, !priority)

# Intentionally use sample_n() even though it's superseded because it allows
# different size per group
to_remove <- if (nrow(over_reps) > 0) {
  over_reps |>
    group_by(division) |>
    mutate(n_keep = n_selected - max_extra_accepted) |>
    sample_n(size = max(n_keep, na.rm = TRUE)) |>
    pull(email_address)
} else {
  ""
}

cohort_selection <- cohort_selection_init |>
  mutate(
    # Move those removed to the end
    number = ifelse(
      email_address %in% to_remove | 
        grepl("fay lab", team_name, ignore.case = TRUE),
      999,
      number
    )
  ) |>
  group_by(cohort) |>
  arrange(number) |>
  mutate(
    number = seq_len(n()),
    accepted = number <= 40,
    status = ifelse(accepted, "accepted", "waitlist"),
    si_name = str_extract(
      tolower(paste(team_name, team_needs, briefly_describe)),
      "(fdd)|(cefi)|(\\bpam\\b)|(\\baa\\b)|(acoustics)|(omics)|(socioecon)|(\\bsi\\b)"
    )
  ) |>
  left_join(
    ss_new |>
      select(email_address, cohort_a, cohort_b, cohort_c),
    by = "email_address"
  ) |>
  select(
    cohort,
    division,
    first_name,
    last_name,
    team_name,
    si_name,
    status,
    everything(),
    -x,
    -accepted,
    -previously_reviewed,
    -none_avail
  )

# Summarize number of each grouping in each cohort
pivot_summary <- function(x, column) {
  x |>
    count(status, {{ column }}, cohort) |>
    pivot_wider(
      names_from = cohort,
      values_from = n,
      names_prefix = "Cohort_",
      values_fill = 0,
    ) |>
    mutate(total = Cohort_A + Cohort_B + Cohort_C) |> 
    arrange(status, {{ column }})
}

accepted_summary <- pivot_summary(cohort_selection, status)
division_summary <- pivot_summary(cohort_selection, division)
si_name_summary <- pivot_summary(cohort_selection, si_name)
team_name_summary <- pivot_summary(cohort_selection, team_name)

## Write to google sheet for final selection
final_worksheet <- paste0("final-selection_", Sys.Date())

sheet_add(signup_sheet_mentor_cp, sheet = final_worksheet)

write_sheet(cohort_selection, ss = signup_sheet_mentor_cp, sheet = final_worksheet)

summary_sheet <- paste0("summary-", final_worksheet)
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

# TODO: 
# - [x] Remove Fay Lab
# - [ ] Summary of total signed up
# - [ ] Write snippet to update summaries as mentors tweak cohort allocations
# - [ ] Make locked "original version"
# - [ ] Add a "selection notes" column
