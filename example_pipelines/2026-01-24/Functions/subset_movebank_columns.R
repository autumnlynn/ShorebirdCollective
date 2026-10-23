# Function to select relevant Movebank columns to move through pipeline
# Movebank attribute dictionary as of 2026-01-24

subset_movebank_columns <- function(df, extra_cols = NULL) {
  # -----------------------------
  # Core columns to keep
  # -----------------------------
  core_cols <- c(
    "event_id",
    "study_id",
    "individual_taxon_canonical_name",
    "individual_local_identifier",
    "individual_id",
    "deployment_id",
    "sensor_type",
    "timestamp",
    "location_long",
    "location_lat",
    "argos_lc",
    "argos_orientation", # relevant Argos columns
    "argos_semi_major",
    "argos_semi_minor",
    "argos_sensor_1", # for assessing dropped tags in PTT-100 tags
    "argos_sensor_2", # for assessing dropped tags in PTT-100 tags
    "argos_sensor_3", # for assessing dropped tags in PTT-100 tags
    "argos_sensor_4", # for assessing dropped tags in PTT-100 tags
    "lotek_crc_status_text", 
    "manually_marked_outlier",
    "sc_download_timestamp",
    "sc_data_receipt_origin",
    "sc_tracking_data_type"
  )
  
  # -----------------------------
  # Include any extra user-specified columns
  # -----------------------------
  if (!is.null(extra_cols)) {
    core_cols <- c(core_cols, extra_cols)
  }
  
  # -----------------------------
  # Select only the columns that exist in the dataset
  # -----------------------------
  df_selected <- df |>
    dplyr::select(dplyr::any_of(core_cols))
  
  return(df_selected)
}
