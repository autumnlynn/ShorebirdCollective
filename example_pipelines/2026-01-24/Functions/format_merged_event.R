# CODE PURPOSE: REORG AND FORMAT MERGED EVENT DAT
# Converts some Movebank columns to factor for sorting as category in arcpro

format_merged_event <- function(df) {
  df |>
    # Rename columns
    dplyr::rename(
      movebank_individual_id = individual_id,
      movebank_deployment_id = deployment_id,
      movebank_study_id = study_id,
      movebank_individual_taxon_canonical_name = individual_taxon_canonical_name # clarify because this is NOT consistent across species all the time
    ) |>
    # Convert to character for ArcGIS Pro sorting
    dplyr::mutate(
      dplyr::across(
        c(movebank_individual_id, movebank_deployment_id, movebank_study_id),
        as.character
      )
    )|>
    # Relocate for more intuitive order:
    dplyr::relocate(species_code, 
             sc_dataset_id, sc_individual_id, sc_deployment_id, 
             individual_local_identifier, sc_tracking_data_type, sc_location_origin) |>
    dplyr::relocate(starts_with("filter"), .after = last_col()) |>
    dplyr::relocate(any_failed_sc_filters, .after = last_col()) |>
    dplyr::relocate(c(sc_download_timestamp, sc_data_receipt_origin, sc_manual_flag_reason,
               sc_dsformat_date, sc_individual_notes), .after = last_col()) |>
    dplyr::relocate(starts_with("movebank_"), .after = last_col()) 
}
