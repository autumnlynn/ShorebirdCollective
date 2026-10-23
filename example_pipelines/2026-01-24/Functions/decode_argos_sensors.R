# This script decodes values in argos sensor columns
# It assumes formulas for PTT-100 tags -- interpret with caution for other argos tag types as different decoding algorithms may be required
# Sensor info including conversion formulas can be found here: https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/atoms/files/MTI_Solar_PTT-100_Manual.pdf
# 
# The following input column names should be (in Movebank format):
## argos_sensor_1, argos_sensor_2, argos_sensor_3, argos_sensor_4
## individual_local_identifier
## timestamp
## location_lat
## location_long
# 
# Allows specification of the deployment, individual, and study names

decode_argos_sensors <- function(
    df,
    deployment_col = deployment_id,   # default movebank col name
    individual_col = individual_id,   # default movebank col name
    study_col = study_id            # default movebank col name
) {
  
  # DETECT ARGOS SENSOR COLUMNS IN EACH DF:
  argos_cols <- names(df)[grepl("^argos_sensor_", names(df))]
  
  # IF NO ARGOS SENSOR COLUMNS EXIST, RETURN EMPTY DF
  if (length(argos_cols) == 0) {
    return(df[0, ])
  }
  
  # FIND DEPLOYMENTS WITH AT LEAST ONE ARGOS SENSOR VALUE:
  deps_with_sensors <- df |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(argos_cols),
        ~ !is.na(.)
      )
    ) |>
    dplyr::distinct({{ deployment_col }}) |>
    dplyr::pull({{ deployment_col }})
  
  # IF NO DEPLOYMENTS HAVE SENSOR DATA, RETURN EMPTY DF
  if (length(deps_with_sensors) == 0) {
    return(df[0, ])
  }
  
  # FILTER & DECODE SENSORS
  df |>
    dplyr::filter(
      ({{ deployment_col }}) %in% deps_with_sensors &
        filter_ll_00 == 1 &
        filter_future_detection == 1 &
        filter_bad_lat == 1 &
        filter_bad_lon == 1 &
        filter_GPS_crc_fail == 1
    ) |>
    dplyr::select(
      {{ study_col}},
      {{ individual_col}},
      individual_local_identifier,
      {{ deployment_col }},
      timestamp,
      location_lat,
      location_long,
      dplyr::contains("argos")
    ) |>
    dplyr::mutate(
      sc_diagnostic_information_type = dplyr::case_when(
        argos_sensor_3 > 0 ~ "normal information frame",
        TRUE ~ "special information frame"
      ),
      sensor_1_internal_temp_c = dplyr::case_when(
        sc_diagnostic_information_type == "normal information frame" ~
          round((0.3609 * argos_sensor_1) - 28.682, 1),
        TRUE ~ NA_real_
      ),
      sensor_2_battery_voltage = dplyr::case_when(
        sc_diagnostic_information_type == "normal information frame" ~
          round((0.0049 * argos_sensor_2) + 3.173, 1),
        TRUE ~ NA_real_
      ),
      sensor_3_counter_per_transmission = dplyr::case_when(
        sc_diagnostic_information_type == "normal information frame" ~ argos_sensor_3,
        TRUE ~ NA_real_
      ),
      sensor_4_activity = dplyr::case_when(
        sc_diagnostic_information_type == "normal information frame" ~ argos_sensor_4,
        TRUE ~ NA_real_
      )
    )
}
