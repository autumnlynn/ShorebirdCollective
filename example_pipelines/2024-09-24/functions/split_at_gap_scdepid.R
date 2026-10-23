# CODE: split_at_gap_depid.R
# Adapted from the initial function from Theo Michelot's momentuHMM workshop:
# Used to segment tracks
# His code is available here: https://github.com/eco4cast/Statistical-Methods-Seminar-Series/blob/main/michelot_movement/code/utility_functions.R
# And below (downloaded 7/25/2022)

# AMA UPDATED THIS TO CHANGE THE EXTENTION FOR THE SEGMENT NAMES TO SAY "_SEG_" then the number
# TO USE "TIMESTAMP" COLUMN INSTEAD OF TIME
# TO USE sc.deployment.id instead of CREATED ID COLUMN

#' Split track at gaps
#' 
#' @param data Data frame with (at least) columns for "ID" and "timestamp"
#' @param max_gap Longest allowed gap, in minutes (track will be split at longer gaps)
#' @param shortest_track Shortest track to keep after splitting, in minutes. Shorter
#' tracks will be removed from the output data set.
#' 
#' @return Data frame with identical structure as input, where ID column
#' has been replaced by new ID for split tracks. Old ID still accessible as
#' ID_old column

split_at_gap_depid <- function(data, max_gap = 60, shortest_track = 0) {
  # Number of tracks
  n_tracks <- length(unique(data$sc.deployment.id))
  
  # Save old ID and reinitialise ID column # AMA SKIP
  # data$ID_old <- data$ID 
  data$segmentID <- character(nrow(data)) 
  
  # Loop over tracks (i.e., over IDs)
  for(i_track in 1:n_tracks) {
    # Indices for this track
    ind_this_track <- which(data$sc.deployment.id == unique(data$sc.deployment.id)[i_track])
    track_length <- length(ind_this_track)
    
    # Time intervals in min
    dtimes <- difftime(data$timestamp[ind_this_track[-1]], 
                       data$timestamp[ind_this_track[-track_length]],
                       units = "mins")
    
    # Indices of gaps longer than max_gap
    ind_gap <- c(0, which(dtimes > max_gap), track_length)
    
    # Create new ID based on split track
    subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
    data$segmentID[ind_this_track] <- paste0(data$sc.deployment.id[ind_this_track], "_SEG_", subtrack_ID)
  }
  
  # Only keep sub-tracks longer than some duration
  track_lengths <- sapply(unique(data$segmentID), function(id) {
    ind <- which(data$segmentID == id)
    difftime(data$timestamp[ind[length(ind)]], data$timestamp[ind[1]], units = "min")
  })
  ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
  data <- subset(data, segmentID %in% ID_keep)
  
  return(data)
}