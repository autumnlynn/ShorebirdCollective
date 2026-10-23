# Function to select relevant Movebank reference columns to move through pipeline
# Movebank library as of 2026-01-24

# skip downloading tag id and tag local identifiers for now because haven't been using them
# animal_nick_name (skip - appears duplicated in animal_local_identifier)
# animal_life_stage (was not that helpful -- either had adult, breeding, migration ASY, SY, AHY (skip for now)
# tag_comments was not very helpful
# tag_failure_comments not very helpful
# In the future, the following columns could be used to help standardize "deployment end type"
## deployment_end_comments 
## deployment_comments?
## animal_comments?
## animal_mortality_date?
## deployment_end_type?
## animal_death_comments?

subset_movebank_reference_columns <- function(df, extra_cols = NULL) {
  # -----------------------------
  # Core columns to keep
  # -----------------------------
  core_cols <- c(
    "study_id",
    "sc_dataset_id", #in direct send data
    "animal_taxon_canonical_name", #individual_taxon_canonical_name in event data
    "species_code", #in direct send data
    "animal_local_identifier", #individual_local_identifier in event data
    "animal_id", # individual_id in event data
    "sc_individual_id", #in direct send data
    "animal_sex",
    "deployment_id",
    "sc_deployment_id",  #in direct send data
    #"sensor_type_id", #numerical identifier: 653 for GPS and 82798 for Argos Doppler Shift (skip and use what we get from event data)
    "sc_tracking_data_type", #in direct send data
    "deploy_on_timestamp",
    "deploy_off_timestamp",
    "deploy_on_latitude",
    "deploy_on_longitude", 
    "deploy_off_latitude",
    "deploy_off_longitude",
    "tag_manufacturer_name",
    "tag_model",
    "tag_weight",
    "duty_cycle", # provided by data owner
    "study_site", # supposed to be for deployment, not always helpful but could provide useful info
    "deployment_end_comments",
    "deployment_comments",
    "animal_comments",
    "animal_mortality_date",
    "deployment_end_type",
    "animal_death_comments",
    "sc_download_timestamp",
    "sc_dsformat_date", # in direct send data
    "sc_data_receipt_origin",
    "sc_tracking_data_type"
  )
  
  # -----------------------------
  # Include any extra user-specified columns
  # -----------------------------
  if (!is.null(extra_cols)) {
    core_cols <- c(core_cols, extra_cols)
  }
  
  # -----------------------------
  # Select only the columns that exist in the dataset
  # -----------------------------
  df_selected <- df |>
    dplyr::select(dplyr::any_of(core_cols))
  
  # -----------------------------
  # Get distinct rows only 
  # -----------------------------
  df_selected <- df_selected |>
    dplyr::distinct()
  
  return(df_selected)
}


# NOTE: IF WE WANT TO CHECK INDIVIDUAL COLUMNS FOR FORMATTING TRY:
# check_df <- ref_dat_list_sub$`study_id`
# check_df |> str()
# 
# # Check for duty_cycle_info:
# check_lst <- ref_dat_list_sub |>
#   purrr::keep(~ "deployment_end_type" %in% names(.x)) |>   # keep only dfs with that column
#   purrr::map(~ dplyr::filter(.x, !is.na(.data$deployment_end_type))) |>
#   purrr::keep(~ nrow(.x) > 0)                      # drop empty dfs
# 
# check_lst |>
#   purrr::map(~.x |> distinct(deployment_end_type))