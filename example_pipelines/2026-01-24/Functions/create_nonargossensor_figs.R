# CREATE DIAGNOSTIC PLOTS FOR BIRDS THAT DO NOT HAVE ARGOS SENSOR DATA FOR
# Helpful for seeing patterns in tag lat/long and LC codes over time
#
# Allow different column name input for deployment id and study id
#
# Input column names need to be:
## argos_sensor_1, argos_sensor_2, argos_sensor_3, argos_sensor_4
## individual_local_identifier
## timestamp
## location_lat
## location_long
## argos_lc

create_nonargossensor_figs <- function(
    df,
    deployment_col = deployment_id, # defaults to movebank column name
    study_col = study_id # defaults to movebank column name
) {
 
  # GET COLUMN NAME AS STRING:
  deployment_str <- rlang::as_string(rlang::ensym(deployment_col))
  
  # FIND ARGOS SENSOR COLUMNS AND GET COLUMN NAMES:
  argos_cols <- names(df)[grepl("^argos_sensor_", names(df))]
  
  # FIND DEPLOYMENTS WITH ARGOS SENSOR DATA (ONLY IF COLUMNS EXIST)
  if (length(argos_cols) > 0) {
    
    deps_with_sensors <- df %>%
      dplyr::filter(
        dplyr::if_any(
          dplyr::all_of(argos_cols),
          ~ !is.na(.)
        )
      ) %>%
      dplyr::distinct({{ deployment_col }}) %>%
      dplyr::pull({{ deployment_col }})
    
  } else {
    
    # IF NO ARGOS COLUMNS EXIST, THEN NONE HAVE SENSOR DATA
    deps_with_sensors <- character(0)
  }
  
  # KEEP ROWS FOR DEPLOYMENTS WITHOUT ARGOS SENSORS AND THAT PASS FILTERS
  df_filtered <- df %>%
    dplyr::filter(
      !({{ deployment_col }}) %in% deps_with_sensors &
        filter_ll_00 == 1 &
        filter_future_detection == 1 &
        filter_bad_lat == 1 &
        filter_bad_lon == 1 &
        filter_GPS_crc_fail == 1
    ) %>%
    dplyr::select(
       {{ study_col }},
      individual_local_identifier,
      {{ deployment_col }},
      timestamp,
      location_lat,
      location_long,
      argos_lc
    )
  
  # SPLIT DATA BY DEPLOYMENT AND KEEP DEPLOYMENT COLUMN
  bird_lst <- df_filtered %>%
    dplyr::group_by( {{ deployment_col }}) %>%
    dplyr::group_split(.keep = TRUE)
  
  # NAME LIST WITH DEPLOYMENT IDS
  bird_names <- df_filtered %>%
    dplyr::distinct( {{ deployment_col }}) %>%
    dplyr::pull( {{ deployment_col }})
  bird_lst <- purrr::set_names(bird_lst, bird_names)
  
  # REMOVE EMPTY DATAFRAMES
  bird_lst <- purrr::keep(bird_lst, ~ nrow(.x) > 0)
  
  # FUNCTION TO CREATE PLOTS PER BIRD
  make_plots <- function(bird_df) {
    
    title <- paste0(
      "Deployment ID: ",
      dplyr::first(bird_df[[deployment_str]]),
      ", Individual: ",
      paste(unique(bird_df %>% dplyr::pull(individual_local_identifier)), collapse = ", ")
    )
    
    # ARRANGE ONCE
    bird_df <- dplyr::arrange(bird_df, timestamp)
    
    # LATITUDE PLOT
    lat_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(x = timestamp, y = location_lat, color = argos_lc),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
      ggplot2::ylab("latitude") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    # LONGITUDE PLOT
    long_plot <- ggplot2::ggplot(bird_df) +
      ggplot2::geom_point(
        ggplot2::aes(x = timestamp, y = location_long, color = argos_lc),
        alpha = 0.5, na.rm = TRUE
      ) +
      viridis::scale_color_viridis(discrete = TRUE, option = "turbo") +
      ggplot2::ylab("longitude") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
    
    # COMBINE LAT/LONG
    diag_plot <- cowplot::plot_grid(
      lat_plot, long_plot,
      ncol = 2, align = "hv"
    )
    
    # TITLE
    title_plot <- cowplot::ggdraw() +
      cowplot::draw_label(
        title,
        x = 0, hjust = 0, size = 12, fontface = "plain"
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 5))
    
    # FINAL COMBINED PLOT
    cowplot::plot_grid(
      title_plot, diag_plot,
      ncol = 1, rel_heights = c(0.12, 1)
    )
  }
  
  # APPLY TO ALL BIRDS
  purrr::map(bird_lst, make_plots)
}
