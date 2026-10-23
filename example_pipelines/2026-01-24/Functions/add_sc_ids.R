# Create and add sc_individual_id and sc_deployment_id columns to the data
# These are helpful because they are unique identifiers that can be used for Movebank datasets and those of other origins (direct send data)
# Requires input of a Movebank event dataframe with the sc_dataset_id column added

add_sc_ids <- function(event_dat) {
  
  # CREATE sc_individual_id
  event_dat_ind_id <- event_dat %>%
    dplyr::mutate(IND = "IND") %>%
    tidyr::unite(
      col = "sc_individual_id",
      c("sc_dataset_id", "IND", "individual_local_identifier"),
      sep = "_",
      remove = FALSE
    ) %>%
    dplyr::select(-IND) %>%
    dplyr::relocate(sc_individual_id, .after = sc_dataset_id)
  
  # CREATE sc_deployment_id
  event_dat_dep_id <- event_dat_ind_id %>%
    dplyr::mutate(DEP = "DEP") %>%
    tidyr::unite(
      col = "sc_deployment_id",
      c("sc_individual_id", "DEP", "deployment_id"),
      sep = "_",
      remove = FALSE
    ) %>%
    dplyr::select(-DEP) %>%
    dplyr::relocate(sc_deployment_id, .after = sc_individual_id)
  
  return(event_dat_dep_id)
}
