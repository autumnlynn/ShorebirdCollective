# Adapted from the initial function from Theo Michelot's momentuHMM workshop:
# https://www.youtube.com/watch?v=WELTpbB5BuU
# Used to segment tracks
# His code is available here: https://github.com/eco4cast/Statistical-Methods-Seminar-Series/blob/main/michelot_movement/code/utility_functions.R
# And below (downloaded 7/25/2022)

# AMA UPDATED THIS TO CHANGE THE EXTENTION FOR THE SEGMENT NAMES TO SAY "_SEG_" then the number
# TO USE "timestamp" COLUMN INSTEAD OF TIME
# Use flexible deployment input column
# and allow input in days

#' Split track at gaps
#' 
#' @param data Data frame with (at least) columns for "ID" and "timestamp"
#' @param max_gap Longest allowed gap, in days (track will be split at longer gaps)
#' @param shortest_track Shortest track to keep after splitting, in days. Shorter
#' tracks will be removed from the output data set.
#' 
#' @return Data frame with identical structure as input, where ID column
#' has been replaced by new ID for split tracks. Old ID still accessible as
#' ID_old column

split_at_gap <- function(data,
                         deployment_col = deployment_id,
                         max_gap = 15,          # in DAYS # split after 15 days gap (allows two weeks which is common in LEYE data)
                         shortest_track = 0) {  # in DAYS (retain all segments)
  
  # Deal with column input to make a string:
  deployment_name <- rlang::as_name(rlang::ensym(deployment_col))
  
  # Force data.frame (avoids tibble method issues)
  data <- as.data.frame(data)
  
  # Extract deployment ID vector
  dep_id <- data[[deployment_name]]
  
  # Initialize segment_id
  data$segment_id <- character(nrow(data))
  
  # Unique deployment IDs
  unique_ids <- unique(dep_id)
  
  for (i in seq_along(unique_ids)) {
    ind_this_track <- which(dep_id == unique_ids[i])
    n_points <- length(ind_this_track)
    
    if (n_points < 2) next
    
    dtimes <- difftime(
      data$timestamp[ind_this_track[-1]],
      data$timestamp[ind_this_track[-n_points]],
      units = "days"
    )
    
    ind_gap <- c(0, which(dtimes > max_gap), n_points)
    
    subtrack_ID <- rep(seq_len(length(ind_gap) - 1), diff(ind_gap))
    
    data$segment_id[ind_this_track] <- paste0(dep_id[ind_this_track], "_SEG_", subtrack_ID)
  }
  
  # Keep only segments longer than shortest_track (days)
  track_lengths <- sapply(unique(data$segment_id), function(id) {
    inds <- which(data$segment_id == id)
    difftime(data$timestamp[inds[length(inds)]], data$timestamp[inds[1]], units = "days")
  })
  
  keep_ids <- names(track_lengths)[track_lengths >= shortest_track]
  data <- data[data$segment_id %in% keep_ids, ]
  
  return(as.data.frame(data))
}
