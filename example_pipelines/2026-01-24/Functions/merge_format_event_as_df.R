# MERGE LISTS OF EVENT DATA ACROSS MOVEBANK/DIRECT SEND STUDIES AFTER APPLYING SC FILTER FLAGS
# REMOVE COLUMNS THAT ARE NOT NEEDED FOR MERGING WITH DIRECT SEND DATA
# Requires list of movebank study event data

# To check unique column names across all datasets:
# dat_list_sub |>
#   purrr::map_dfr(~.x |> colnames() |> as.data.frame()) |>
#   distinct(.)

merge_format_event_as_df <- function(dat_list_sub) {
  
  # VECTOR OF COLUMNS TO DROP IF THEY EXIST
  cols_to_drop <- c(
    "argos_sensor_1", "argos_sensor_2", "argos_sensor_3", "argos_sensor_4",
    "manually_marked_outlier", "lotek_crc_status_text", "ctt_error_code"
  )
  
  # DROP, RENAME, COERCE TYPES (IF PRESENT), AND MERGE
  dat_merge_df <- purrr::map_dfr(
    dat_list_sub,
    ~ .x |>
      dplyr::select(-dplyr::any_of(cols_to_drop)) |>
      base::as.data.frame()
  )
  
  return(dat_merge_df)
}
