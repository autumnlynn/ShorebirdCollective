# FOR GPS DATA, ADDS IDENTIFIER OF "G" INTO argos_lc COLUMN...
# ...This is an accepted format in Movebank and
# ...the format is also used in AniMotum
#
# INPUT DF REQUIRES THE FOLLOWING COLUMNS IN MOVEBANK FORMAT:
# sensor_type with "Argos Doppler Shift" or "GPS" as options
# argos_lc 

add_argos_lc_for_gps <- function(df) {

  # -----------------------------
  # Ensure argos_lc column exists
  # -----------------------------
  if (!"argos_lc" %in% names(df)) {
    df <- df |> dplyr::mutate(argos_lc = NA_character_)
  }
  
  # -----------------------------
  # Update argos_lc to "G" for GPS rows
  # -----------------------------
  df <- df |>
    dplyr::mutate(argos_lc = case_when(
      sensor_type == "GPS" ~ "G",
      TRUE ~ argos_lc
    ))
  
  return(df)
}
