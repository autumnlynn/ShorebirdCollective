# Flag events outside of Shorebird Collective identified deployment start and end times
# These are essentially revisions to the the deployment start or end times provided by the data owner found during data exploration...
# OR new start/end times if they were not provided by the data owner
# 0 = FAIL
# 1 = PASS
# Allows input for different deployment id column names if needed
#
# REQUIRES 2 DATAFRAME INPUTS:
# 1) DF of event data with the following columns (in Movebank format)
## timestamp
# 2) DF with sc flagged on and off times & the following columns:
## sc_deploy_on_timestamp
## sc_deploy_off_timestamp
# DF format is one row per deployment_id

flag_sc_deploy_times <- function(dat_df, sc_deps_df,
                                 dat_deploy_col = deployment_id, # deployment id in the points data df: defaults to Movebank deployment_id column
                                 sc_deploy_col  = deployment_id) { # deployment id in the csv containing the revised start and end times (default uses Movebank deployment_id 
  
  # PREPARE COLUMN NAMES FOR STRING INPUTS:
  dat_deploy_str <- rlang::as_string(rlang::ensym(dat_deploy_col))
  sc_deploy_str  <- rlang::as_string(rlang::ensym(sc_deploy_col))
  
  sc_on_col  <- "sc_deploy_on_timestamp"
  sc_off_col <- "sc_deploy_off_timestamp"
  
  # JOIN BOTH DF'S BY DEPLOYMENT_ID
  dat_df %>%
    dplyr::left_join(
      sc_deps_df %>%
        dplyr::select(dplyr::all_of(c(sc_deploy_str, sc_on_col, sc_off_col))),
      by = setNames(sc_deploy_str, dat_deploy_str)
    ) %>%
    
    # ADD COLUMNS FOR SPECIFYING IF TIMESTAMP IS OUTSIDE SC DEPLOYMENT TIMES
    dplyr::mutate(
      filter_sc_deploy_on_time  = dplyr::if_else(
        dplyr::coalesce(timestamp >= .data[[sc_on_col]], TRUE), # NA's pass
        1L, 0L 
      ),
      filter_sc_deploy_off_time = dplyr::if_else(
        dplyr::coalesce(timestamp <= .data[[sc_off_col]], TRUE),
        1L, 0L
      )
    ) %>%
    dplyr::select(-dplyr::all_of(c(sc_on_col, sc_off_col)))
}
