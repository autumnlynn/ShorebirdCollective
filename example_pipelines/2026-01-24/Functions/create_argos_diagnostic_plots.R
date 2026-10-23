# CREATE DIAGNOSITIC PLOTS OF ARGOS SENSORS FOR TROUBLE SHOOTING
# Helpful for assessing tag performance and possible drop off/death of animal (if sensor data available)
#
# This is currently set up to only create figures for individuals with data in argos_sensor columns
# It uses decoded sensor data from the script decode_argos_sensors.r
# ... which decodes sensor columns assuming the formulas for PTT-100 tags -- interpret with caution for other argos tag types as different decoding algorithms may be required
# Sensor info including conversion formulas can be found here: https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/MTI_Solar_PTT-100_Manual.pdf
#
# This script allows input of any deployment column but defaults to Movebank's deployment_id
#
# Requires the following column names (in Movebank format):
## individual_local_identifier
## timestamp
## location_lat
## location_long
## argos_lc
## sensor_1_internal_temp_c (from decoded sensor script)
## sensor_2_battery_voltage (from decoded sensor script)
## sensor_4_activity (from decoded sensor script)

create_argos_diagnostic_plots <- function(df, deployment_col = deployment_id) {
  
  # GET STRING NAME OF COLUMN FOR REQUIRED STRING INPUTS:
  dep_str <- rlang::as_string(rlang::ensym(deployment_col))
  
  # CREATE SEPARATE LIST PER BIRD
  bird_lst <- split(df, df[[dep_str]])
  
  make_plots <- function(bird_df) {
    
    # Arrange once
    bird_df <- dplyr::arrange(bird_df, timestamp)
    
    title <- paste0(
      "Deployment ID: ", unique(bird_df[[dep_str]]),
      ", Individual: ", paste(unique(bird_df$individual_local_identifier), collapse = ", ")
    )
    
    lat_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(timestamp, location_lat, color = argos_lc),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
      ggplot2::ylab("latitude") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    long_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(timestamp, location_long, color = argos_lc),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
      ggplot2::ylab("longitude") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    temp_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(timestamp, sensor_1_internal_temp_c,
                     color = sensor_1_internal_temp_c),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = FALSE, option = "rocket") +
      ggplot2::ylab("tag temperature (C)") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    batt_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(timestamp, sensor_2_battery_voltage,
                     color = sensor_2_battery_voltage),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = FALSE, option = "mako") +
      ggplot2::ylab("battery voltage") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    activity_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(timestamp, sensor_4_activity,
                     color = sensor_4_activity),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = FALSE, option = "cividis") +
      ggplot2::ylab("activity") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    diag_plot <- cowplot::plot_grid(
      lat_plot, long_plot, temp_plot, batt_plot, activity_plot,
      ncol = 2, align = "hv"
    )
    
    title_plot <- cowplot::ggdraw() + 
      cowplot::draw_label(title, x = 0, hjust = 0, size = 12) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 5)
      )
    
    cowplot::plot_grid(
      title_plot, diag_plot,
      ncol = 1, rel_heights = c(0.12, 1)
    )
  }
  
  purrr::map(bird_lst, make_plots)
}