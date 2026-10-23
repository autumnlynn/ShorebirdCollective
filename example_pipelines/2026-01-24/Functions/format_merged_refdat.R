# CODE PURPOSE: REORG AND FORMAT MERGED EVENT DAT
# Converts some Movebank columns to factor for sorting as category in arcpro

format_merged_refdat <- function(df) {
  df |>
    # Rename columns
    dplyr::rename(
      movebank_individual_id = individual_id,
      movebank_deployment_id = deployment_id,
      movebank_study_id = study_id,
      movebank_individual_taxon_canonical_name = individual_taxon_canonical_name # clarify because scientific names are NOT always consistent across species in Movebank
    ) |>
    # Convert to character to match the merged event data (category for arcpro)
    dplyr::mutate(
      dplyr::across(
        c(movebank_individual_id, movebank_deployment_id, movebank_study_id),
        as.character
      )
    )|>
    # Relocate for more intuitive order:
    dplyr::relocate(species_code, 
                    sc_dataset_id, sc_individual_id, sc_deployment_id, 
                    individual_local_identifier, sc_tracking_data_type) |>
    dplyr::relocate(c(sc_download_timestamp, sc_data_receipt_origin,
                      sc_dsformat_date), .after = last_col()) |>
    dplyr::relocate(starts_with("movebank_"), .after = last_col()) |>
    dplyr::relocate(starts_with("directsend_"), .after = last_col())
}
