# Extract deployment lat and long on locations and timestamps from Movebank Reference Data
# Only extract if lat/long columns have at least 3 digits (~111m accuracy)
#
# REQUIRES THE FOLLOWING INPUT COLUMNS IN MOVEBANK FORMAT:
# deploy_on_timestamp
# deploy_on_latitude
# deploy_on_longitude
# study_id
# animal_taxon_canonical_name
# animal_id
# deployment_id

# ACCEPTS THE FOLLOWING INPUT MOVEBANK COLUMNS IF THEY EXIST:
# animal_local_identifier

# AND COLUMNS CREATED BY SHOREBIRD COLLECTIVE:
# sc_download_timestamp 
# sc_data_receipt_origin


# CUSTOM FUNCTION TO COUNT NUMBER OF DECIMALS IN GEOGRAPHIC COORDINATES IN DECIMAL DEGREES
# Note: assumes trailing 0's don't count
count_decimals <- function(x) {
  if (length(x) == 0) return(numeric())
  
  # Convert each number to text and count number of characters
  x_nchr <- x |> abs() |> as.character() |> nchar() |> as.numeric()
  # Count the number of integer values before decimal using the same approach (but rounding down to nearest integer)
  x_int  <- floor(x) |> abs() |> as.character() |> nchar()
  # Subtract the integer count from the total number of characters (and subtract 1 to remove the decimal count)
  x_nchr <- x_nchr - 1 - x_int
  # Replace negative numbers with 0
  x_nchr[x_nchr < 0] <- 0
  # Return n decimal numbers
  x_nchr
}


# FUNCTION TO EXTRACT DEPLOY-ON LOCATIONS:
extract_deploy_on_locations <- function(df) {
  
  # SPECIFY COLUMNS NEEDED FROM THE REFERENCE DATA
  required_cols <- c(
    "deploy_on_timestamp",
    "deploy_on_latitude",
    "deploy_on_longitude"
  )
  
  # If required columns don't exist → return empty tibble
  if (!all(required_cols %in% colnames(df))) {
    return(dplyr::tibble())
  }
  
  # FILTER AND SELECT COLUMNS:
  df |>
    dplyr::filter(number_of_events > 0) |>   # removes deps with out and data and NA automatically
    dplyr::select(
      study_id,
      animal_taxon_canonical_name,
      animal_id,
      sc_download_timestamp,
      sc_data_receipt_origin,
      deployment_id,
      dplyr::any_of(c(
        "animal_local_identifier", #not present in all dfs
        "deploy_on_timestamp",
        "deploy_on_latitude",
        "deploy_on_longitude"
      ))
    ) |>
    # FORMAT TIMESTAMPS
    dplyr::distinct() |>
    dplyr::mutate(
      deploy_on_timestamp = lubridate::ymd_hms(deploy_on_timestamp, tz = "UTC"),
      dep_lat_digits  = count_decimals(deploy_on_latitude),
      dep_long_digits = count_decimals(deploy_on_longitude)
    ) |>
    # GET LOCATIONS WITH ONLY HIGH ACCURACY:
    dplyr::filter(
      dep_lat_digits >= 3,
      dep_long_digits >= 3,
      !is.na(deploy_on_timestamp)
    ) |>
    # SPECIFY THAT THESE HAVE GPS LEVEL ACCURACY
    dplyr::mutate(
      argos_lc = "G",
      sc_location_origin = "deploy_on_lat_long"
    ) |>
    # RENAME COLUMNS TO MATCH MOVEBANK EVENT DATA
    dplyr::rename(
      timestamp = deploy_on_timestamp,
      location_lat = deploy_on_latitude,
      location_long = deploy_on_longitude,
      individual_id = animal_id,
      individual_taxon_canonical_name = animal_taxon_canonical_name
    ) |>
    # RENAME ANIMAL LOCAL IDENTIFIER (IF IT EXISTS IN THE DF)
    dplyr::rename(
      dplyr::any_of(c(
        individual_local_identifier = "animal_local_identifier"
      ))
    ) |>
    # ENSURE TAHT IND LOCAL IDENTIFIER IS A CHARACTER (FOR MERGING)
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of("individual_local_identifier"),
        as.character
      )
    ) |>
    dplyr::select(-dep_lat_digits, -dep_long_digits)
}
