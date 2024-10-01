is_true_v <- Vectorize(isTRUE)

read_signup_sheet <- function(x, ...) {
  sheet <- read_sheet(x, skip = 6, .name_repair = name_cleaner, ...)
  sheet |>
    mutate(
      across(matches("^(cohort_)|(email)"), tolower),
      across(where(is.logical), is_true_v) # convert NAs into FALSEs
    )
}

name_cleaner <- function(x) {
  x <- janitor::make_clean_names(x)
  nms_tokens <- strsplit(x, "_")
  vapply(
    nms_tokens,
    \(x) {
      if (length(x) < 2) {
        return(x)
      }
      paste(x[1:2], collapse = "_")
    },
    FUN.VALUE = ""
  )
}

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