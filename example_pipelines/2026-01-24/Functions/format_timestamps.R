# PARSE TIMESTAMPS
# IF PARSING FAILS BECAUSE THE TIMESTAMP DOES NOT HAVE HH:MM:SS...
## SET HH:MM:SS TO 12:00:00 ON THAT DAY (minimizes difference to 12 hours in either direction)
#
# Input df needs columns (movebank format):
## timestamp

format_timestamps <- function(df, deployment_col = deployment_id) { #default of movebank format
  
  # -----------------------------
  # GET ROWS WHERE TIMESTAMPS PARSE CORRECTLY
  # -----------------------------
  df_ts <- df |>
    dplyr::mutate(
      timestamp = suppressWarnings(lubridate::ymd_hms(timestamp, tz = "UTC"))
    ) |>
    dplyr::filter(!is.na(timestamp))
  
  # -----------------------------
  # FIX TIMESTAMPS WHERE HOURS, MIN, SEC ARE MISSING
  # Assume midday (12:00:00)
  # -----------------------------
  df_ts_fix <- df |>
    dplyr::mutate(
      ts_hr  = lubridate::hour(timestamp),
      ts_min = lubridate::minute(timestamp),
      ts_sec = lubridate::second(timestamp)
    ) |>
    dplyr::filter(ts_hr == 0, ts_min == 0, ts_sec == 0) |>
    dplyr::mutate(hms = "12:00:00") |>
    tidyr::unite("timestamp", c("timestamp", "hms"), sep = " ") |>
    dplyr::mutate(
      timestamp = suppressWarnings(lubridate::ymd_hms(timestamp, tz = "UTC"))
    ) |>
    dplyr::select(-ts_hr, -ts_min, -ts_sec)
  
  # -----------------------------
  # JOIN BACK TOGETHER AND ARRANGE
  # -----------------------------
  df_clean <- dplyr::bind_rows(df_ts, df_ts_fix) |>
    dplyr::arrange({{ deployment_col }}, timestamp)
  
  return(df_clean)
}