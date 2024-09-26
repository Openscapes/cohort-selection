# NMFS 2024 Champions Cohort selection

This is a set of scripts to assist in the selection of people to participate
in one of three Champions Cohorts in Fall 2024, offered as part of the
NMFS-Openscapes collaboration.

We used a three part process:

1. Create a google sheet with applicants' information from their sign-ups, and
merge this with other sources of information that would help inform the selection
process. Eg:

- Whether or not the applicant is a supervisor or mentor
- If they have participated in an Openscapes Champions program previously
- If they are part of a NOAA Strategic Initiative
- If they are a new hire

The code to create this sheet is contained in `01-nmfs-cohort-merge.R`

Mentors were then given an opportunity to review this sheet to ensure the information
for the selection criteria was correct.

2. Using the annotated google sheet with the selection criteria, generate
a proposed cohort of 40 people for each cohort (A,B,C). The lists were generated
by prioritizing people based on the input criteria, and assignment to cohorts was
based on stated availability for each cohort, with randomization. Additional
spots were filled randomly based on cohort availability, and rules were imposed
to try to ensure fair distribution of spots across divisions.

This sheet was created using `02-nmfs-cohort-selection.R`, and uploaded to Google
Sheets.

3. Mentors reviewed this sheet and made suggested adjustments, and final selections
were made by the Openscapes team.

`03-summaries.R` was used to create summaries of the allocation of seats by cohort,
division, Strategic Initiative, and team.
