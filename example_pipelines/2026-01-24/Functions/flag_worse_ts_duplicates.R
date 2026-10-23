# WHEN THERE ARE TWO OR MORE GEOGRAPHIC LOCATIONS AT THE SAME TIMESTAMP WITHIN A DEPLOYMENT,
# FLAG THE LOWER QUALITY LOCATIONS FOR REMOVAL
# 0 = FAIL
# 1 = PASS
#
# Rationale, prefilter to select the higher quality location
# Currently aniMotum flags duplicates, but it does not flag a higher quality location -- it keeps the first
#
# Input the deployment_col column specific to that dataset, defaults to Movebank's deployment_id
#
# Input df requires the following columns (in Movebank format)
## argos_lc
## timestamp
## argos_semi_major
## argos_semi_minor

flag_worse_ts_duplicates <- function(df, 
                                     deployment_col = deployment_id) { # default movebank deployment ID
  
  # -----------------------------
  # Convert deployment_col to string for string inputs
  # -----------------------------
  deployment_name <- rlang::as_string(rlang::ensym(deployment_col))
  
  # -----------------------------
  # Add temporary row ID to safely join later
  # -----------------------------
  df <- dplyr::mutate(df, .row_id = dplyr::row_number())
  
  # -----------------------------
  # Ensure Argos LC ordering from low to high quality
  # -----------------------------
  if ("argos_lc" %in% names(df)) {
    lc_levels <- c("B", "A", "0", "1", "2", "3", "G")
    df <- dplyr::mutate(
      df,
      argos_lc = factor(argos_lc, levels = lc_levels, ordered = TRUE)
    )
  }
  
  # -----------------------------
  # Find duplicated timestamps per deployment
  # -----------------------------
  dup_df <- df |>
    dplyr::filter(any_failed_sc_filters == "FALSE") |>
    janitor::get_dupes({{ deployment_col }}, timestamp)
  
  # If no duplicates, return all rows flagged 1 (or PASS)
  if (nrow(dup_df) == 0) {
    df <- dplyr::mutate(df, filter_worse_ts_duplicate = 1)
    df <- dplyr::select(df, - .row_id)
    return(df)
  }
  
  # -----------------------------
  # Assign duplicate IDs
  # -----------------------------
  dup_ids <- dup_df |>
    dplyr::select({{ deployment_col }}, timestamp) |>
    dplyr::distinct() |>
    dplyr::mutate(dup_id = dplyr::row_number())
  
  dup_df <- dplyr::left_join(
    dup_df,
    dup_ids,
    by = c(deployment_name, "timestamp")
  )
  
  # -----------------------------
  # Arrange duplicates by quality
  # -----------------------------
  # If best argos_lc, argos_semi_major, argos_semi_minor columns exist,
  # ... order by highest LC code, smallest semi_major axis distance, and smallest semi_major axis distance
  if (all(c("argos_semi_major", "argos_semi_minor") %in% names(dup_df))) {
    dup_df <- dplyr::arrange(
      dup_df,
      {{ deployment_col }},
      dup_id,
      dplyr::desc(argos_lc),
      argos_semi_major,
      argos_semi_minor
    )
    # If only the argos_lc and semi_major column exists order by highest lc_quality
    # ... then order by smallest semi_major axis
  } else if ("argos_semi_major" %in% names(dup_df)) {
    dup_df <- dplyr::arrange(
      dup_df,
      {{ deployment_col }},
      dup_id,
      dplyr::desc(argos_lc),
      argos_semi_major
    )
    # If only the argos_lc is available, order by highest LC
  } else {
    dup_df <- dplyr::arrange(
      dup_df,
      {{ deployment_col }},
      dup_id,
      dplyr::desc(argos_lc)
    )
  }
  
  # -----------------------------
  # Flag worse duplicates
  # -----------------------------
  dup_df <- dup_df |>
    dplyr::group_by({{ deployment_col }}, dup_id) |>
    dplyr::slice(-1) |>   # keep best row, flag the rest
    dplyr::ungroup() |>
    dplyr::mutate(filter_worse_ts_duplicate = 0)
  
  # -----------------------------
  # Join back to original df using .row_id
  # -----------------------------
  df <- dplyr::left_join(
    df,
    dup_df |> dplyr::select(.row_id, filter_worse_ts_duplicate),
    by = ".row_id"
  )
  
  # -----------------------------
  # Fill in 1 for non-flagged rows
  # -----------------------------
  df <- df |>
    dplyr::mutate(
      filter_worse_ts_duplicate = ifelse(is.na(filter_worse_ts_duplicate), 1, filter_worse_ts_duplicate)
    )
  
  # -----------------------------
  # Remove temporary row ID
  # -----------------------------
  df <- dplyr::select(df, - .row_id)
  
  return(df)
}