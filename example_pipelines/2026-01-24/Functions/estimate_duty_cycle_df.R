# Estimate duty cycles for each tag
# This is draft code and is somewhat clunky
# It is a coarse way to try and determine broad duty cycle or tag programming timesteps
# It will not work well for tags that continuously collect data (e.g. Sunbird tags)
# ...or tags with irregular sampling intervals
# For tags like Sunbirds, it will tell you the most common time interval between geographic locations AFTER removing those that occur...
# ... within argos_min_gap
#
# Allows flexible input for deployment_id column
#
# Requires the following columns (in Movebank format)
## timestamp
## individual_local_identifier
# 
# Requires sc_tracking_data_type column
## Created earlier in the pipeline 
## Has values of either argos, gps, or argosgps and is the type of data recorded by the tag overall
#
# argos_min_gap is the amount of time elapsed in minutes between subsequent geographic locations that are disregarded...
# ... when calculating the median/mean sampling intervals
# ... we chose to disregard a certain amount of time because when argos tags are continuously collecting data...
# ... there are lots of very short gaps between locations and including these can make it difficult to identify the next most common gap intervals
# ... most duty cycled tags in this dataset are programmed for 10 hours on...
# ... so we set the default argos_min_gap to 600 minutes (to disregard those very short gaps)
# 
# NOTE: The mean and median sampling interval reported for tags collecting ARGOS DATA ONLY are...
# ... those calculated AFTER excluding intervals less than argos_min_gap 
# If looking for the true mean and median sampling interval, it will be necessary to change the reporting structure

estimate_duty_cycle_df <- function(df,
                                   deployment_col = deployment_id, # default is Movebank deployment_id
                                   argos_min_gap = 600) {
  
  # ---- SPECIFY MODE FUNCTION ----
  getmode <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NA_real_)
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # ---- CALCULATE TIME ELAPSED ----
  time_elapsed_df <- df |>
    dplyr::filter(any_failed_sc_filters != "TRUE") |>
    dplyr::arrange({{ deployment_col }}, timestamp) |>
    dplyr::group_by({{ deployment_col }}) |>
    dplyr::mutate(
      elapsed_sensor_types = paste0(dplyr::lag(sensor_type), ", ", sensor_type),
      elapsed_mins = plyr::round_any(
        as.numeric(
          difftime(
            timestamp,
            dplyr::lag(timestamp, default = dplyr::first(timestamp)),
            units = "mins"
          )
        ),
        10 # round to nearest 10 min
      )
    ) |>
    dplyr::ungroup()
  
  # ---- SUMMARISE GAPS ----
  summarise_gaps <- function(data, type_label) {
    if (nrow(data) == 0) return(NULL)
    
    data |>
      dplyr::group_by(individual_local_identifier, {{ deployment_col }}, sc_tracking_data_type) |>
      dplyr::arrange(elapsed_mins) |>
      dplyr::summarise(
        mean_gap_mins   = round(mean(elapsed_mins, na.rm = TRUE), 0),
        mode_gap_mins   = getmode(elapsed_mins),
        median_gap_mins = median(elapsed_mins, na.rm = TRUE),
        n_obs           = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        mode_gap_hrs    = round(mode_gap_mins / 60, 1),
        median_gap_hrs  = round(median_gap_mins / 60, 1),
        mode_gap_days   = round(mode_gap_hrs / 24, 1),
        median_gap_days = round(median_gap_hrs / 24, 1),
        
        estimated_duty_cycle_mins = dplyr::case_when(
          median_gap_mins > 0 ~ median_gap_mins,
          median_gap_mins == 0 & mean_gap_mins > 0 ~ mean_gap_mins,
          TRUE ~ NA_real_
        ),
        
        estimated_duty_cycle_hrs  = round(estimated_duty_cycle_mins / 60, 0),
        estimated_duty_cycle_days = round(estimated_duty_cycle_mins / 60 / 24, 0),
        
        sc_estimated_duty_cycle_description = dplyr::case_when(
          is.na(estimated_duty_cycle_mins) ~ "insufficient data to estimate",
          estimated_duty_cycle_mins < 60 ~ paste0(type_label, " every ", estimated_duty_cycle_mins, " minutes"),
          estimated_duty_cycle_mins < 1440 ~ paste0(type_label, " every ", estimated_duty_cycle_hrs, " hours"),
          TRUE ~ paste0(type_label, " every ", estimated_duty_cycle_days, " days")
        )
      )
  }
  
  # ---- TAGS WITH GPS ONLY ----
  gps_df <- time_elapsed_df |>
    dplyr::filter(sc_tracking_data_type == "gps")
  gps_est <- summarise_gaps(gps_df, "1 transmission")
  
  # ---- TAGS WITH ARGOS ONLY ----
  argos_df <- time_elapsed_df |>
    # FILTER OUT ELPASED TIME INTERVALS < argos_min_gap BEFORE SUMMARISING
    dplyr::filter(sc_tracking_data_type == "argos",
                  elapsed_mins > argos_min_gap)
  argos_est <- summarise_gaps(argos_df, "multiple transmissions")
  
  # ---- TAGS WITH ARGOS + GPS ----
  argosgps_df <- time_elapsed_df |>
    dplyr::filter(
      sc_tracking_data_type == "argosgps",
      elapsed_sensor_types %in% c("GPS, Argos Doppler Shift", "GPS, GPS")
    )
  argosgps_est <- summarise_gaps(argosgps_df, "1 or 2 transmissions")
  
  # ---- COMBINE ALL RESULTS ----
  result <- dplyr::bind_rows(gps_est, argos_est, argosgps_est)
  
  return(result)
}
