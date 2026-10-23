# FLAG LOCATIONS LIKELY TO OCCUR DURING TEST PERIODS AT MANUFACTURER FACILITY
# DEFAULT IS FOR MICROWAVE TELEMETRY FACILITY BUT COULD BE UPDATED IF ANOTHER SITE BECOMES A PROBLEM
#
# Looks for locations near facility (default 1 km) and ...
# ...gets locations within the first 10 days of the first location there
# ...This time period was chosen because all tags we have found at test facilities occur within a period of 10 days
# Ideally, this code would not be needed because we would have proper deployment on and off times
# ... yet somehow these slip through the cracks
#
# Requires the following input columns (Movebank format):
## location_long
## location_lat
## timestamp
#
# REQUIRES THE split_at_gap.r function to be loaded
source("./Code/Functions/split_at_gap.r")

flag_manufacturer_locations <- function(dat_df,
                                        deployment_col = deployment_id,  # Defaults to Movebank deployment_id
                                        manufacturer_lat = 39.23119,
                                        manufacturer_lon = -76.81563,
                                        buffer_near_m = 1000, # Buffer distances around manufacturer location to look for points
                                        buffer_far_m = 50000,
                                        gap_days = 30, # split at gap days
                                        extend_days = 10, # days to filter out locations at test site
                                        crs = "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs") {
  # -----------------------------
  # Deployment column names as strings
  # -----------------------------
  deployment_sym <- rlang::ensym(deployment_col)        # for !! unquoting
  deployment_str <- rlang::as_string(deployment_sym)   # for all_of() and column selection
  
  # -----------------------------
  # Helper function to expand date ranges
  # -----------------------------
  fill_dates <- function(df) {
    tibble::tibble(
      !!deployment_sym := df[[deployment_str]],
      date = seq(as.Date(df$start), as.Date(df$end), by = "1 day")
    )
  }
  
  # -----------------------------
  # Manufacturer location as sf
  # -----------------------------
  manufacturer_sf <- data.frame(
    lat = manufacturer_lat,
    lon = manufacturer_lon
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = sf::st_crs(crs))
  
  buffer_1km <- sf::st_buffer(manufacturer_sf, buffer_near_m)
  buffer_50km <- sf::st_buffer(manufacturer_sf, buffer_far_m)
  
  # -----------------------------
  # Convert data to sf
  # -----------------------------
  dat_sf <- dat_df |>
    dplyr::filter(any_failed_sc_filters != "TRUE") |>
    dplyr::select(dplyr::all_of(c(deployment_str, "location_long", "location_lat", "timestamp"))) |>
    sf::st_as_sf(coords = c("location_long", "location_lat"), crs = 4326) |>
    sf::st_transform(crs = sf::st_crs(crs))
  
  # -----------------------------
  # Find deployments detected within 1 km
  # -----------------------------
  bird_ids_1km <- sf::st_filter(dat_sf, buffer_1km) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::distinct(.data[[deployment_str]]) |>
    dplyr::pull()
  
  if (length(bird_ids_1km) == 0) {
    dat_df$filter_manufacturer_loc <- 1
    return(dat_df)
  }
  
  # -----------------------------
  # Get all detections within 50 km for those birds
  # -----------------------------
  points_50km <- sf::st_filter(dat_sf, buffer_50km) |>
    sf::st_drop_geometry() |>
    as.data.frame() |>
    dplyr::filter(.data[[deployment_str]] %in% bird_ids_1km) |>
    dplyr::arrange(.data[[deployment_str]], timestamp) |>
    dplyr::select(dplyr::all_of(c(deployment_str, "timestamp")))
  
  # -----------------------------
  # Split by deployment and segment by gaps
  # -----------------------------
  points_per_bird <- base::split(points_50km, points_50km[[deployment_str]]) |>
    purrr::map(as.data.frame)  # ensures base data.frames
  
  segmented_df <- purrr::map_dfr(
    points_per_bird,
    ~ split_at_gap(.x, deployment_col = deployment_str, max_gap = gap_days, shortest_track = 0)
  )
  
  # -----------------------------
  # Keep only segments with >1 observation
  # -----------------------------
  valid_segments <- segmented_df |>
    dplyr::count(segment_id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(segment_id)
  
  manuf_date_ranges <- segmented_df |>
    dplyr::filter(segment_id %in% valid_segments) |>
    dplyr::group_by(segment_id) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      start = as.Date(timestamp),
      end = start + extend_days
    ) |>
    dplyr::select(dplyr::all_of(c(deployment_str, "start", "end", "segment_id")))
  
  if (nrow(manuf_date_ranges) == 0) {
    dat_df$filter_manufacturer_loc <- 1
    return(dat_df)
  }
  
  # -----------------------------
  # Expand date ranges
  # -----------------------------
  filled_dates <- base::split(manuf_date_ranges, manuf_date_ranges$segment_id) |>
    purrr::map_dfr(fill_dates) |>
    dplyr::mutate(filter_manufacturer_loc = 0)
  
  # -----------------------------
  # Join back to original data safely
  # -----------------------------
  dat_df <- dat_df |>
    dplyr::mutate(date = as.Date(timestamp)) |>
    dplyr::left_join(filled_dates, by = c(deployment_str, "date")) |>
    dplyr::mutate(
      filter_manufacturer_loc = dplyr::if_else(is.na(filter_manufacturer_loc), 1L, filter_manufacturer_loc)
    ) |>
    dplyr::select(-date)
  
  # -----------------------------
  # Return final dataset
  # -----------------------------
  return(dat_df)
}