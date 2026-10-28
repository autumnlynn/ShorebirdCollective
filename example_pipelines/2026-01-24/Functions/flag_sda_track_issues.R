# Function with input of:
# 1) sda filtered sf from aniMotum
# 2) original sc prefiltered sf
#
# The function flags short duration tag deployments (<= 7 days) 
# and those with few obs (<= 4) after prefiltering, so they do not run through the SSM
# Note: even higher thresholds may be necessary, this simply allows us to pull the most tracking data 
# ... through the SSM

source("./Code/Functions/flag_failed_sc_filters.r")

flag_sda_track_issues <- function(sda_filtered_sf, sc_prefiltered_sf) {
  
  ## 1) GET SDA FILTERED TAG DEPLOYMENT TRACK DURATIONS ----
  track_duration_df <- sda_filtered_sf |> 
    sf::st_drop_geometry() |>
    dplyr::arrange(sc_deployment_id, timestamp) |>
    dplyr::group_by(sc_deployment_id) |> 
    dplyr::filter(dplyr::row_number() == 1 | dplyr::row_number() == dplyr::n()) |> 
    dplyr::mutate(
      seq_id = dplyr::row_number(),
      first_last_elapsed_mins = round(
        as.numeric(
          difftime(
            timestamp,
            dplyr::lag(timestamp, default = first(timestamp)),
            units = "mins"
          )
        ), 0
      )
    ) |>
    dplyr::select(sc_deployment_id, seq_id, first_last_elapsed_mins) |>
    dplyr::arrange(sc_deployment_id, seq_id) |>
    dplyr::filter(seq_id == 2) |>
    dplyr::ungroup()
  
  
  ## 2) IDS WITH TRACK LENGTH <= 7 DAYS ----
  short_duration_IDs <- track_duration_df |>
    dplyr::filter(first_last_elapsed_mins <= 10080) |>  # 7 days
    dplyr::pull(sc_deployment_id)
  
  
  ## 3) FLAG SHORT TRACKS IN ORIGINAL DATASET ----
  event_sc_prefiltered_sf_sdaflags <- sc_prefiltered_sf |>
    dplyr::mutate(
      filter_track_duration_less8days =
        dplyr::case_when(
          sc_deployment_id %in% short_duration_IDs ~ 0,
          .default = 1
        )
    )
  
  
  ## 4) COUNT LOCATIONS PASSING THE SDA ----
  dep_visible_counts_df <- sda_filtered_sf |>
    sf::st_drop_geometry() |>
    dplyr::group_by(sc_deployment_id) |>
    dplyr::count() |>
    dplyr::rename(n_visible_detections = n)
  
  
  ## 5) IDS WITH <= 4 PASSING LOCATIONS ----
  short_count_IDs <- dep_visible_counts_df |>
    dplyr::filter(n_visible_detections <= 4) |>
    dplyr::pull(sc_deployment_id)
  
  
  ## 6) FLAG TAG DEPLOYMENTS WITH LOW SAMPLE SIZES AFTER SDA IN PREFILTERED DATA----
  event_sc_prefiltered_sf_sdaflags <- event_sc_prefiltered_sf_sdaflags |>
    dplyr::mutate(
      filter_visible_obs_less5 =
        dplyr::case_when(
          sc_deployment_id %in% short_count_IDs ~ 0,
          .default = 1
        )
    )
  
  
  ## 7) GET TAG DEPLOYMENTS WITH < 3 LOCATIONS PRIOR TO SDA ----
  scprefilt_dep_id_less3 <- sc_prefiltered_sf |>
    sf::st_drop_geometry() |>
    dplyr::group_by(sc_deployment_id) |>
    dplyr::count() |>
    dplyr::filter(n < 3) |>
    dplyr::pull(sc_deployment_id)
  
  
  ## 8) FLAG SC PREFILTER TAG DEPLOYMENTS WITH <3 LOCATIONS PRIOR TO SDA ----
  # FOR RECORD KEEPING
  event_sc_prefiltered_sf_sdaflags <- event_sc_prefiltered_sf_sdaflags |>
    dplyr::mutate(
      filter_scprefilter_less3 =
        dplyr::case_when(
          sc_deployment_id %in% scprefilt_dep_id_less3 ~ 0,
          .default = 1
        )
    )
  
  
  ## 9) APPLY EXISTING FAILED FILTER FUNCTION ----
  event_sc_prefiltered_sf_sdaflags <- event_sc_prefiltered_sf_sdaflags |>
    flag_failed_sc_filters() |>
    dplyr::mutate(
      any_failed_sc_filters =
        dplyr::case_when(
          is.na(any_failed_sc_filters) ~ "FALSE",
          .default = "TRUE"
        )
    )
  
  return(event_sc_prefiltered_sf_sdaflags)
}
