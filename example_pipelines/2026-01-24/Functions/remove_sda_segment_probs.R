# Function with input of:
## 1) sda filtered sf 
## 2) list of data input into animotum prefilter in the merc180 projection
#
# Purpose: Finds segment_ids with short duration (<= 7 days) and few obs (<= 4) after sda prefiltering...
# ...for removal prior to running SSM
# Removes those segment_id from the input dataframe
# Creates several outputs:
## 1) aniMotum input data with the short segments removed (for running in SSM)
## 2) vector of segment_id s removed
## 3) vector of segment_id s that were short duration
## 4) vector of segment_id s that had 4 or fewer points
#
# Really a larger threshold of number of points to retain would be more suitable

remove_sda_segment_probs <- function(prefiltered_sda_daily_sf,
                                   input_merc180_dat_lst,
                                   duration_threshold_mins = 10080,  # <=7 days
                                   count_threshold = 4) { #<= 4 obs
  
  # 1) Track durations
  seg_duration_df <- prefiltered_sda_daily_sf |>
    sf::st_drop_geometry() |>
    dplyr::arrange(segment_id, timestamp) |>
    dplyr::group_by(segment_id) |>
    dplyr::filter(dplyr::row_number() == 1 | dplyr::row_number() == dplyr::n()) |>
    dplyr::mutate(
      seq_id = dplyr::row_number(),
      first_last_elapsed_mins = round(
        as.numeric(
          difftime(
            timestamp,
            dplyr::lag(timestamp, default = first(timestamp)),
            units = "mins"
          )
        ), 0
      )
    ) |>
    dplyr::select(segment_id, seq_id, first_last_elapsed_mins) |>
    dplyr::arrange(segment_id, seq_id) |>
    dplyr::filter(seq_id == 2) |>
    dplyr::ungroup()
  
  
  # 2) IDs with short duration
  short_duration_IDs <- seg_duration_df |>
    dplyr::filter(first_last_elapsed_mins <= duration_threshold_mins) |>
    dplyr::pull(segment_id)
  
  
  # 3) Count detections
  dep_visible_counts_df <- prefiltered_sda_daily_sf |>
    sf::st_drop_geometry() |>
    dplyr::group_by(segment_id) |>
    dplyr::count() |>
    dplyr::rename(n_visible_detections = n)
  
  
  # 4) IDs with few detections
  short_count_IDs <- dep_visible_counts_df |>
    dplyr::filter(n_visible_detections <= count_threshold) |>
    dplyr::pull(segment_id)
  
  
  # 5) Combine IDs to remove
  remove_IDs <- unique(c(short_duration_IDs, short_count_IDs))
  
  
  # 6) Remove from animotum input list
  input_merc180_dat_lst_checked <- input_merc180_dat_lst |>
    purrr::map(~ dplyr::filter(.x, !(id %in% remove_IDs)))
  
  
  return(list(
    cleaned_data = input_merc180_dat_lst_checked,
    removed_ids = remove_IDs,
    short_duration_ids = short_duration_IDs,
    short_count_ids = short_count_IDs
  ))
}
