# Add Shorebird Collective dataset metadata columns: sc_dataset_id and species_code to df
## sc_dataset_id is a unique dataset identifier we pair with data sharing agreement terms
## species_code is helpful for keeping track throughout the remainder of the pipeline
#
# It's important to add sc_dataset_id to
# 1) pair with data sharing agreement terms specific to the dataset
# 2) create internal sc_individual_id and sc_deployment_id later in pipeline 
# ...these are unique ID's used after mergining with data not housed in Movebank (direct send data)
#
# The function requires 2 inputs:
# 1) datasets_argosgps_meta_df : a dataframe of Shorebird Collective metadata containing:
## species_code 
## sc_dataset_id
## sc_tracking_data_type (argos, gps, or argosgps)
## movebank_individual_taxon_canonical_name (the scientific name as used in Movebank) (NOTE: this name may be out of date)
## movebank_id (the Movebank Study id)
#
# 2) dat_merge_df : event df of geographic locations with flags for removal containing:
## sc_tracking_data_type (argos, gps, or argosgps)
## study_id (Movebank study_id)
## individual_taxon_canonical_name (Movebank scientific name)
# 
# NOTE: Each sc_dataset_id is associated with an sc_tracking_data_type (argos, gps, or argosgps)
# This is because sometimes different data types within a Movebank study have sub-ownership ...
# ... or different data sharing agreement terms
# When mapping, some tags sc_tracking_data_type == argos only or gps only will not map with their sc_dataset_id because...
# ... the tags were supposed to collect argos and gps data, so they are flagged as (argosgps) in the dataset metadata ...
# ... This is more likely to happen for tags transmitting for a short period of time 
# ... This function specifically addresses this by searching for tags that are not assigned an sc_dataset_id after left_join...
# ... and filling those with the sc_dataset_id for study_id and species labeled with sc_tracking_datatype == argosgps in the dataset metadata

add_sc_dataset_ids <- function(datasets_argosgps_meta_df, dat_merge_df) {
  
  # SUBSET SHOREBIRD COLLECTIVE METADATA TO SPECIFIC COLUMNS 
  datasets_df <- datasets_argosgps_meta_df |>
    dplyr::filter(sc_data_receipt_origin == "Movebank") |>
    dplyr::select(
      sc_dataset_id, # internal to SC
      movebank_id, # for mapping
      sc_tracking_data_type, # argos, gps, or argosgps (the type of data collected by tags in the dataset)
      movebank_individual_taxon_canonical_name, # scientific name used by Movebank for that species 
      species_code # 4 letter code
    ) |>
    dplyr::rename(
      study_id = movebank_id, # to map with movebank format
      individual_taxon_canonical_name = movebank_individual_taxon_canonical_name # to map with movebank format
    )
  
  # FIND STUDIES, SPECIES, AND TAG TRACKING DATA TYPES WHERE sc_dataset_id is not filled in the event data
  datasets_unassigned <- dplyr::left_join(dat_merge_df, datasets_df) |>
    dplyr::filter(is.na(sc_dataset_id)) |>
    dplyr::distinct(study_id, individual_taxon_canonical_name, sc_tracking_data_type) 
  
  # DROP TRACKING TYPE TO CHECK STUDIES AND SPECIES WITH DATASETS INITIALLY UNASSIGNED: 
  datasets_to_check <- datasets_unassigned |>
    dplyr::select(-sc_tracking_data_type)
  
  # GET ROWS FOR THOSE STUDIES AND SPECIES THAT ALREADY EXIST IN THE METADATA AS sc_tracking_data_type ARGOSGPS:
  dataset_rows_to_add <- dplyr::right_join(datasets_df, datasets_to_check) |>
    dplyr::filter(sc_tracking_data_type == "argosgps") |>
    dplyr::select(-sc_tracking_data_type)
  
  # ADD MISSING SOLO TRACKING TYPES AS AN OPTION TO FACILITATE MERGING: 
  dataset_rows_to_add <- dplyr::left_join(dataset_rows_to_add, datasets_unassigned)
  
  # APPEND NEW ROWS TO DATASET DF:
  datasets_df <- dplyr::bind_rows(datasets_df, dataset_rows_to_add)
  
  # JOIN DATASET ASSIGNMENTS BACK TO EVENT DATA:
  dat_merge_df_datasets <- dplyr::left_join(dat_merge_df, datasets_df)
  
  return(dat_merge_df_datasets)
}
