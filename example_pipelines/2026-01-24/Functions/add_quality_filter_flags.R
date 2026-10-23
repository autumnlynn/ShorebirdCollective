# FLAG COMMON LOCATION DATA PROBLEMS FOR REMOVAL
# 0 = FAIL
# 1 = PASS
# Creates a separate column for each type of flag
#
# Requires columns (in movebank format):
## location_lat
## location_long
## timestamp
## argos_lc
#
## Allows input of the following columns if they exist
## lotek_crc_status_text
## manually_marked_outlier

# Can include the following column added as a flag by Shorebird Collective for direct send data:
## ctt_error_code


add_quality_filter_flags <- function(df) {
  
  # -----------------------------
  # Ensure required optional columns exist
  # -----------------------------
  # Create and fill with NA
  if (!"lotek_crc_status_text" %in% names(df)) {
    df$lotek_crc_status_text <- NA_character_
  }
  
  if (!"manually_marked_outlier" %in% names(df)) {
    df$manually_marked_outlier <- NA_character_
  }
  
  if (!"ctt_error_code" %in% names(df)) {
    df$ctt_error_code <- NA_real_
  }
  
  # -----------------------------
  # Add all filter flags 
  # -----------------------------
  df |>
    dplyr::mutate(
      
      # Incomplete case
      filter_incomplete_case =
        dplyr::case_when(
          is.na(location_lat) |
            is.na(location_long) |
            is.na(timestamp) ~ 0,
          TRUE ~ 1
        ),
      
      # No LC class
      filter_no_lc_class =
        dplyr::case_when(
          argos_lc == "" | is.na(argos_lc) ~ 0,
          TRUE ~ 1
        ),
      
      # GPS CRC fail
      filter_GPS_crc_fail =
        dplyr::case_when(
          lotek_crc_status_text == "Fail" ~ 0,
          TRUE ~ 1
        ),
      
      # Future timestamp
      filter_future_detection =
        dplyr::case_when(
          timestamp > lubridate::now() ~ 0,
          TRUE ~ 1
        ),
      
      # Manually marked outlier
      filter_manually_marked_outlier =
        dplyr::case_when(
          manually_marked_outlier == "true" ~ 0,
          TRUE ~ 1
        ),
      
      # Bad latitude
      filter_bad_lat =
        dplyr::case_when(
          location_lat > 90 | location_lat < -90 ~ 0,
          TRUE ~ 1
        ),
      
      # Bad longitude
      filter_bad_lon =
        dplyr::case_when(
          location_long > 180 | location_long < -180 ~ 0,
          TRUE ~ 1
        ),
      
      # Argos Z
      filter_argos_z =
        dplyr::case_when(
          argos_lc == "Z" ~ 0,
          TRUE ~ 1
        ),
      
      # CTT error
      filter_ctt_error =
        dplyr::case_when(
          ctt_error_code != 0 ~ 0,
          TRUE ~ 1
        ),
      
      # Lat/Long at 0,0
      filter_ll_00 =
        dplyr::case_when(
          round(location_lat, 0) == 0 &
            round(location_long, 0) == 0 ~ 0,
          TRUE ~ 1
        )
    )
}