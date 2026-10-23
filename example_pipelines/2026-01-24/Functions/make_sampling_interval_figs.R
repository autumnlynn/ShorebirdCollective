# Make plots of frequency of sampling intervals of for each tag deployment
# Creates list of figs, one for each deployment
#
# Input is a dataframe with the following columns (in Movebank format):
## timestamp
## sensor_type
## individual_local_identifier
#
# Allows flexible input for deployment id column name
# 
# This code calculates the number of minutes elapsed between subsequent geographic locations in a tag deployment
# It rounds those observations to the nearest 10 minute bin
# And it assesses which type of sensors were used to record the two geographic locations (creates a column elapsed_sensor_types)
# Then it counts the number of observations (pairs of geographic locations where time elapsed was calculated) in each 10 interval
# Finally it creates plots of the frequency of observations in the 10 minute bin and colors by the type of sensors used to record each geographic location
# Peaks in these plots can indicate duty cycling or tag programming at specific time periods

make_sampling_interval_figs <- function(df, 
                                        deployment_col = deployment_id, # default of is Movebank deployment_id
                                        max_minutes = 20160) { # max duration to show on the x axis in minutes, default figures show 2 weeks (in min)
  
  # Capture column (works with bare or string input)
  deployment_str <- rlang::as_string(rlang::ensym(deployment_col))
  
  # ---- CALCULATE TIME & SENSORS ELAPSED ----
  time_elapsed_df <- df |>
    dplyr::filter(any_failed_sc_filters != "TRUE") |> 
    dplyr::arrange({{ deployment_col }}, timestamp) |>
    dplyr::group_by({{ deployment_col }}) |>
    dplyr::mutate(
      elapsed_mins = plyr::round_any(
        as.numeric(difftime(
          timestamp,
          dplyr::lag(timestamp, default = dplyr::first(timestamp)),
          units = "mins"
        )),
        10
      ),
      elapsed_mins_fact = as.factor(elapsed_mins),
      elapsed_sensor_types = paste0(lag(sensor_type), ", ", sensor_type)
    ) |>
    dplyr::ungroup()
  
  # ---- COUNT EVENTS PER 10-MIN BIN ----
  minute_count_df <- time_elapsed_df |>
    dplyr::group_by(individual_local_identifier,
                    {{ deployment_col }},
                    elapsed_mins_fact,
                    elapsed_sensor_types) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      elapsed_mins = as.numeric(as.character(elapsed_mins_fact))
    ) |>
    dplyr::rename(n_instances = n)
  
  # ---- SPLIT BY DEPLOYMENT ----
  minute_count_lst <- split(minute_count_df,
                            minute_count_df[[deployment_str]])
  
  # ---- CREATE TITLES ----
  plot_title_lst <- purrr::map(minute_count_lst, ~ paste0(
    "Deployment ID: ",
    dplyr::first(.x[[deployment_str]]),
    " | Individual: ",
    paste(unique(.x$individual_local_identifier), collapse = ", ")
  ))
  
  # ---- CREATE PLOTS ----
  plot_list <- purrr::map2(
    minute_count_lst,
    plot_title_lst,
    ~ .x |>
      dplyr::filter(!(elapsed_sensor_types %in% 
                        c("NA, Argos Doppler Shift", "NA, GPS"))) |>
      ggplot2::ggplot(ggplot2::aes(
        x = elapsed_mins,
        y = n_instances,
        color = elapsed_sensor_types
      )) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::theme_bw() +
      ggplot2::xlim(c(0, max_minutes)) +
      ggplot2::labs(
        title = .y,
        color = "sensor types (elapsed)",
        x = "mins elapsed between each detection",
        y = "number of observations"
      ) +
      ggplot2::theme(legend.position = "bottom")
  )
  
  return(plot_list)
}
