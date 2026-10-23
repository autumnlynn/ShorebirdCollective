# ADD SEQUENTIAL ROW NUMBER (ID) FOR EACH DEPLOYMENT (AFTER ON AND OFF LOCATIONS ARE ADDED)
#
# Requires timestamp column 
# Allows flexible deployment id but defaults to Movebank deployment_id

add_sequential_ids <- function(df, deployment_col = deployment_id) { #if a value isn't provided will default to deployment_id
 
  
  # # -----------------------------
  # # Add year, month, date columns
  # # -----------------------------
  # df <- df |>
  #   dplyr::mutate(
  #     detection_year  = as.character(lubridate::year(timestamp)),
  #     detection_month = as.character(lubridate::month(timestamp)),
  #     detection_doy = lubridate::yday(timestamp),
  #     date = as.Date(timestamp)
  #   )
  
  # -----------------------------
  # Add sequential observation ID per deployment
  # -----------------------------
  df <- df |>
    dplyr::group_by({{ deployment_col }}) |>
    dplyr::arrange({{ deployment_col }}, timestamp) |>
    dplyr::mutate(orig_dep_seq_id = row_number()) |>
    dplyr::ungroup()
  
  return(df)
}
