# Function to combine input data and output after the aniMotum prefilter into a sf object...
# And filter data to those that passed the prefilter
#
# The function preserves the original input data columns ...
# ... and appends the output of "keep" column from the sda filter to it...
# ... then filters to only those that passed the filter...
# ... and adds tag metadata back in

get_sda_prefiltered_data <- function(output_df_lst, input_df_lst, metadata_df) {
  
  ## 1) FORMAT OUTPUT DFS ----
  fulloutput_df <- output_df_lst |>
    purrr::map_dfr(~ .x |>
                     sf::st_drop_geometry() |>
                     dplyr::select(id, date, keep) |>
                     dplyr::rename(sda_prefilter_tf = keep) |>
                     as.data.frame()) |>
    dplyr::arrange(id, date)
  
  
  ## 2) FORMAT INPUT DFS ----
  input_df <- input_df_lst |>
    purrr::map_dfr(~ .x |> as.data.frame()) |>
    dplyr::arrange(id, date)
  
  
  ## 3) JOIN "KEEPS" COLUMN WITH INPUT DATA ----
  input_plus_sdafilter_df <- cbind(
    input_df,
    fulloutput_df |> dplyr::select(sda_prefilter_tf)
  )
  
  
  ## 4) FILTER TO THOSE THAT PASSED SDA FILTER AND CONVERT TO SF ----
  dat_sda_filtered_sf <- input_plus_sdafilter_df |>
    dplyr::filter(sda_prefilter_tf == "TRUE") |>
    dplyr::rename(
      sc_deployment_id = id,
      timestamp = date,
      argos_lc = lc,
      location_long = lon,
      location_lat = lat,
      argos_semi_major = smaj,
      argos_semi_minor = smin,
      argos_orientation = eor
    ) |>
    sf::st_as_sf(coords = c("location_long", "location_lat"),
                 crs = "EPSG:4326") |>
    dplyr::mutate(
      location_long = sf::st_coordinates(.) [,1],
      location_lat  = sf::st_coordinates(.) [,2]
    )
  
  
  ## 5) ADD METADATA ----
  dat_sda_filtered_sf_meta <- dplyr::left_join(
    dat_sda_filtered_sf,
    metadata_df,
    by = dplyr::join_by(sc_deployment_id)
  )
  
  return(dat_sda_filtered_sf_meta)
}
