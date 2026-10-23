# Flag any outlier events found in previous investigations by the Shorebird Collective
# Flags with:
# 1 = PASS FILTER
# 0 = FAIL FILTER
# 
# Requires an input dataframe with the event_id from Movebank (a unique record identifier, to flag for removal
# And a column called: sc_manual_flag_reason
# ...this column describes the reason the event_id was flagged for removal 
# These are often runs of spike points that are not correct (and not removed later in the pipeline with a sda filter)

flag_movebank_sc_mmo <- function(df, sc_mmo_df) {
  
  df <- dplyr::left_join(
    df,
    sc_mmo_df |> dplyr::select(event_id, sc_manual_flag_reason),
    by = "event_id"
  )
  
  # Ensure the column exists, create as NA_integer if missing
  if (!"sc_manual_flag_reason" %in% colnames(df)) {
    df$sc_manual_flag_reason <- NA_integer_
  }
  
  # Add the flag column
  df <- dplyr::mutate(
    df,
    filter_sc_manually_marked_outlier = dplyr::case_when(
      !is.na(sc_manual_flag_reason) ~ 0L,
      TRUE ~ 1L
    )
  )
  
  df <- as.data.frame(df)
  return(df)
}