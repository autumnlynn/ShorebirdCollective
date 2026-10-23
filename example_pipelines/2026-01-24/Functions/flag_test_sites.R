# Flag sites where tags were tested before or after deployment for removal
# These are problem sites not currently captured by deployment on/off timestamps and locations
# These are sites where tags were tested prior to deployment (often at someone's house, airport, fieldstation, etc.)
#
# Requires two data frames as inputs:
# 1) dat_df of point locations with the following columns (in Movebank format)
## location_lat
## location_long
## orig_dep_seq_id (created by add_sequential_ids earlier in script 03)
#
# 2) site_df with locations of the test sites with the following columns:
## lat (in decimal degrees)
## lon (in decimal degrees)
## requires a row per site
#
# This script looks for points within a 5 km buffer (as default) around known tag test sites and 
# ...flags them for removal
# Currently this is set up to look flag points only for species with tags known to be tested at these sites
# ... that way if other birds move through the 5 km area, their points are not removed
# Ideally this script would not be necessary with accurate tag deployment on/off timestamps and locations
# ... but problems persist
#
# Allows different input column names for deployment and species columns
# Note, sometimes species scientific names are out of date in Movebank

flag_test_sites <- function(dat_df,
                            site_df,
                            buffer_m = 5000, # default buffer distance to check for points within
                            species_keep = c("Numenius americanus", "Haematopus palliatus", "Limosa fedoa"),
                            species_col = individual_taxon_canonical_name,  # defaults to Movebank's species scientific names
                            deployment_col = deployment_id,               # defaults to Movebank's deployment_id
                            proj_crs = "+proj=laea +lon_0=-101.6 +lat_0=19.22 +datum=WGS84 +units=m +no_defs") { # Projection should be in meters
  
  # -----------------------------
  # Create column string
  # -----------------------------
  deployment_str <- rlang::as_string(rlang::ensym(deployment_col))
  
  # -----------------------------
  # Convert site_df to sf and buffer by specified distance
  # -----------------------------
  site_sf <- site_df |>
    sf::st_as_sf(coords = c("lon","lat"), crs = 4326) |>
    sf::st_transform(crs = sf::st_crs(proj_crs)) |>
    sf::st_buffer(buffer_m)
  
  # -----------------------------
  # Convert point data to sf
  # -----------------------------
  dat_sf <- dat_df |>
    dplyr::filter(filter_incomplete_case == 1) |>  # keep only complete points
    sf::st_as_sf(coords = c("location_long","location_lat"), crs = 4326) |>
    sf::st_transform(crs = sf::st_crs(site_sf))
  
  # -----------------------------
  # Identify points intersecting buffered sites
  # -----------------------------
  in_sites_sf <- sf::st_filter(dat_sf, site_sf, .predicate = sf::st_intersects) |>
    dplyr::mutate(filter_test_sites = 0L)
  
  # -----------------------------
  # Filter for target species only
  # -----------------------------
  in_sites <- in_sites_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter({{ species_col }} %in% species_keep)
  
  # -----------------------------
  # Join back to original data
  # -----------------------------
  dat_df <- dat_df |>
    dplyr::mutate(filter_test_sites = 1L) |>
    dplyr::left_join(
      in_sites |>
        dplyr::select(orig_dep_seq_id, {{ deployment_col }}, timestamp, filter_test_sites),
      by = c("orig_dep_seq_id", deployment_str, "timestamp"),
      suffix = c("", ".new")
    ) |>
    dplyr::mutate(filter_test_sites = dplyr::coalesce(filter_test_sites.new, filter_test_sites)) |>
    dplyr::select(-filter_test_sites.new)
  return(dat_df)
}
