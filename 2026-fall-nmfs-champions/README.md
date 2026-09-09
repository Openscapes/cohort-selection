# NMFS Fall 2026 Champions Cohort selection

This is a set of scripts to assist in the selection of people to participate in one of three NMFS-Openscapes Champions Cohorts in Fall 2026.

Signups are collected in the "Fall 2026 Champions" tab of the `Champions_Interest_Fall2026` workbook.

**Status: complete.** The final approved assignments (104 people; Cohort A = 32, B = 37, C = 35) live in the `cohort-picks` sheet of the signup workbook and have been copied into the participant-facing `ParticipantsList` workbook.

1. `01-nmfs-cohorts.R` reads the signup sheet and:
   - Does some data cleaning/normalization.
   - Assigns each team to whichever cohort it has the strongest combined preference for (proportion "yes" + half-weight for "unsure"), breaking ties randomly, so teams stay together as much as possible.
   - Assigns individuals not on a team: among the cohorts each person said "yes" to, it picks the one with the most remaining capacity; only when no "yes" cohort has room does it fall back to "unsure" cohorts, using the same room-based rule.
     This guarantees nobody lands in an "unsure" cohort while a "yes" cohort of theirs still had space.
   - Flags rows needing a manual look (`needs_review`): individuals with no usable preference at all, and cases where a team's pick landed on a cohort a member personally said "no" to.
   - Produces summary tables (cohort counts, division x cohort, team rosters) for review.
     The write block is commented out; it was run once to create the `cohort-picks` sheet, after which the picks were manually reviewed and edited.
     Do not re-run it: the sheet now intentionally differs from what the script would produce.

2. `02-post-cohort-edit.R` reads the reviewed/edited `cohort-picks` sheet back and regenerates the summary tables, plus a check that no team ended up split across cohorts after manual edits.

3. `03-write-cohort-sheet.R` copies the final assignments from `cohort-picks` into the `ParticipantsList` workbook:
   - The three `2026-nmfs-champions-{a,b,c}` tabs: one roster per cohort, written below each tab's existing header row (the `*github_username` column is left for participants to fill in).
   - `2026-all`: the union of the three cohorts.
   - `SeasideChatFormations`: one row per team with more than one member -- team name in "Peer Group Name (aka Team)", member names in "Peer Group/Team members".
     Description and scheduling columns are left blank for teams to fill in.

   **Do not re-run** once participants have entered data: roster rows would shift and leave their github/scheduling values paired with the wrong people.
   Copy and adapt for the next season instead.
