source("R/header.R")
source("R/functions.R")

ss_raw <- read_signup_sheet(signup_sheet)
ss_mentors <- read_signup_sheet(signup_sheet_mentor_cp)

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
    by = c("email_address" = "email"), na_matches = "never"
  ) |>
  mutate(
    prev_champion = !is.na(prev_cohort)
  ) |>
  left_join(
    ss_mentors |>
      # keep all mentor-populated columns, and add "previously_reviewed" column
      select(
        email_address,
        julie_steph = julie_and,
        eli_comments,
        mentor_comments,
        new_hire,
        si,
        supervisor,
        mentor,
        override_yes
      ) |> 
      mutate(
        previously_reviewed = TRUE
      ),
    by = "email_address"
  ) |>
  mutate(
    si = si | grepl(
      "(fdd)|(cefi)|(\\bsi\\b)|(\\bpam\\b)|(\\baa\\b)|(acoustics)|(omics)|(socioecon)",
      paste(team_name, team_needs, briefly_describe),
      ignore.case = TRUE
    ),
    new_hire = new_hire | grepl("new\\s+hire", team_needs, ignore.case = TRUE),
    previously_reviewed = is_true_v(previously_reviewed),
    notes = NA_character_
  )

## Find those who didn't choose yes for any cohort
# ss_raw |>
#   filter(!grepl("yes", cohort_a), !grepl("yes", cohort_b), !grepl("yes", cohort_c)) |>
#   pull(email_address) |>
#   cat(sep = ", ")

## Write to google sheet for further annotation
sheet_add(signup_sheet_mentor_cp, sheet = annotated_ws)

comments_df <- data.frame(
  comment = c(
    "Check and update values in columns `prev_cohort`:`override_yes`. These have been populated from lists of previous Champions cohorts, the previously-annotated Fall 2024 Champions sheet (mentors copy), and by parsing certain fields to extract information (e.g., SI teams)",
    "The `override_yes` column should be set to `TRUE` if the person should be added to a cohort regardless of other variables considered."
  )
)

range_write(
  signup_sheet_mentor_cp,
  data = comments_df,
  sheet = annotated_ws,
  range = "A1",
  col_names = FALSE
)

range_write(
  signup_sheet_mentor_cp,
  data = ss,
  sheet = annotated_ws,
  range = paste0("A", nrow(comments_df) + 2)
)
