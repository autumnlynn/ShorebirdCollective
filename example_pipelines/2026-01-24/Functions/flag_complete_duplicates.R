# FLAG ONE of TWO ROWS THAT ARE COMPLETE DUPLICATES FOR REMOVAL
# 0 = FAIL
# 1 = PASS
#
# Allows flexible column name input for event_id, a unique row identifier for a dataset
# Defaults to using the Movebank column event_id
#
# Requires the column orig_dep_seq_id (created with add_sequential_ids in 03 script)
# 
# Note: if a row is completely duplicated except for lotek_crc_status_text, it will not be flagged with 0 here
# This could be updated because those with duplicated text appear to be complete duplicates but have not cross checked

flag_complete_duplicates <- function(df, 
                                     event_col = event_id) { #default movebank format
 
  # Convert bare column name text for operations that require column name as a string (text) instead of input as full column
  event_name <- rlang::as_name(rlang::ensym(event_col))
  
  # -----------------------------
  # Define columns to exclude from duplicate check: these are columns that are unique to the event and filter columns
  # -----------------------------
  exclude_cols <- c(
    event_name,  
    "orig_dep_seq_id",
    names(df)[grepl("filter", names(df))], # any that are already columns flagging for filtering
    names(df)[grepl("^sc_", names(df))] # any with the prefix "sc_", columns added by Shorebird Collective
  )
  
  # -----------------------------
  # Find duplicates
  # -----------------------------
  dup_df <- janitor::get_dupes(df, -dplyr::all_of(exclude_cols))
  
  # If no duplicates, ensure column exists and return
  if (nrow(dup_df) == 0) {
    df <- dplyr::mutate(df, filter_complete_duplicate = 1)
    return(df)
  }
  
  # -----------------------------
  # Assign duplicate IDs
  # -----------------------------
  dup_ids <- dup_df |>
    dplyr::select(-dplyr::all_of(exclude_cols)) |>
    dplyr::distinct() |>
    dplyr::mutate(dup_id = 1:dplyr::n())  
  
  dup_df <- dplyr::left_join(
    dup_df,
    dup_ids,
    by = base::intersect(names(dup_df), names(dup_ids))
  )
  
  # -----------------------------
  # Flag all rows but the first row per duplicate group
  # -----------------------------
  dup_df <- dup_df |>
    dplyr::select(-dupe_count) |>
    dplyr::group_by(dup_id) |>
    dplyr::slice(2:dplyr::n()) |>  # flag 0 for all except first row
    dplyr::ungroup() |>
    dplyr::mutate(filter_complete_duplicate = 0)
  
  # -----------------------------
  # Join back to original
  # -----------------------------
  df <- dplyr::left_join(
    df,
    dup_df |> dplyr::select(dplyr::all_of(event_name), filter_complete_duplicate),
    by =  event_name
  )
  
  # -----------------------------
  # Fill in 1 for rows not flagged
  # -----------------------------
  if (!"filter_complete_duplicate" %in% names(df)) {
    df <- dplyr::mutate(df, filter_complete_duplicate = NA_integer_)
  }
  
  df <- df |>
    dplyr::mutate(
      filter_complete_duplicate = dplyr::case_when(
        is.na(filter_complete_duplicate) ~ 1,
        TRUE ~ filter_complete_duplicate
      )
    )
  
  return(df)
}
