#' Create a Track object
#'
#' The `Track` function creates an object of class "Track" and "BirdFlowRoute".
#' 
#' @rdname TrackDataClass
#' @param track_id A string or number representing the unique identifiers of the track.
#' @param track_date A vector representing the dates associated with the track.
#' @param track_longitude A vector of longitudes for the track points.
#' @param track_latitude A vector of latitudes for the track points.
#' @param track_x A vector of x-coordinates in a projected coordinate system (projected into BirdFlow CRS).
#' @param track_y A vector of y-coordinates in a projected coordinate system (projected into BirdFlow CRS).
#' @param track_type A character string indicating the type of track (default: "Undefined").
#' @param track_info Additional information or metadata about the track (default: "").
#'
#' @return An object of class "Track" and "BirdFlowRoute".
#' @export
Track <- function(track_id, track_date, track_longitude, track_latitude, track_x, track_y, track_type = "Undefined", track_info="") {
  # check input data format
  check_data_input_format.Track(track_id, track_date, track_longitude, track_latitude, track_x, track_y, track_type, track_info)
  
  # 
  data <- list(track_id=track_id,
               track_date=track_date,
               track_longitude=track_longitude,
               track_latitude=track_latitude,
               track_x=track_x,
               track_y=track_y,
               track_type=track_type,
               track_info=track_info)
  structure(
    data, class = c("Track", "BirdFlowRoute")
  )
}

#' Check the input data format of Track object
#' 
#' @rdname TrackDataClass
#' @export
check_data_input_format.Track <- function(track_id, track_date, track_longitude, track_latitude, track_x, track_y, track_type, track_info){
  if ((!is.character(track_id) & !is.numeric(track_id)) || length(track_id) != 1){
    stop(sprintf("'track_id' must be a single string or number"))
  }
  if (
    (!inherits(track_date, "Date")) || any(is.na(track_date))
  ) {
    stop("'track_date' must be of class 'Date'. Please provide valid Date objects.")
  }
  for (var_name in c("track_longitude", "track_latitude", "track_x", "track_y")) {
    var <- get(var_name)
    
    if (!is.numeric(var) || any(is.na(var))) {
      stop(sprintf("'%s' must be a numeric vector and cannot contain NA values.", var_name))
    }
    
    if (length(var) != length(track_date)) {
      stop(sprintf("All variables must have the same length! '%s' must have the same length as 'track_date'.", var_name))
    }
  }
  
  if (max(track_longitude) > 180 || min(track_longitude) > 180){
    stop(sprintf("'track_longitude' not in range (-180, 180)!"))
  }
  if (max(track_latitude) > 90 || min(track_latitude) < -90){
    stop(sprintf("'track_latitude' not in range (-90, 90)!"))
  }
  valid_track_types <- c("Tracking", "Banding", "BirdFlow_generated_route","Undefined")
  if (!is.character(track_type) || length(track_type) != 1 || !(track_type %in% valid_track_types)) {
    stop(sprintf("'track_type' must be one of: %s", paste(valid_track_types, collapse = ", ")))
  }
  
  if (!is.character(track_info)){
    stop(sprintf("'info' must be a string."))
  }
}


#' Print the Track object
#' 
#' This function print the track as a summary
#' 
#' @rdname TrackDataClass
#' @param track An object of class "Track".
#' @return No return value. Prints a summary to the console.
#' @method print Track
#' @export
print.Track <- function(track) {
  pad_width <- 18
  cat(format("Track ID: ", width = pad_width), track$track_id, "\n")
  cat(format("Number of points: ", width = pad_width), length(track$track_date), "\n")
  cat(format("Date range: ", width = pad_width), format(min(track$track_date)), "to", format(max(track$track_date)), "\n")
  cat(format("Longitude range: ", width = pad_width), range(track$track_longitude), "\n")
  cat(format("Latitude range: ", width = pad_width), range(track$track_latitude), "\n")
  cat(format("x range: ", width = pad_width), range(track$track_x), "\n")
  cat(format("y range:", width = pad_width), range(track$track_y), "\n")
  cat(format("Track type: ", width = pad_width), track$track_type, "\n")
  cat(format("Info: ", width = pad_width), ifelse(length(track$track_info)>20, paste0(substr(track$track_info, 1, 20),' ...'), track$track_info), "\n") # only print the head 20 characters
}


#' Create a TrackCollection object
#'
#' The `TrackCollection` function creates an object of class "TrackCollection".
#' 
#' @rdname TrackDataClass
#' @param tracks a list of objects of class "Track". Default to empty list.
#' @return An object of class "TrackCollection" containing a list of tracks (default: empty list).
#' @export
TrackCollection <- function(tracks = list()) {
  structure(
    list(tracks = tracks),
    class = "TrackCollection"
  )
}

#' Check if an object is a valid Track for TrackCollection
#'
#' This function checks whether the input is a valid object of class "Track".
#' 
#' @rdname TrackDataClass
#' @param track An object to check.
#' @return No return value. Raises an error if the input is not of class "Track".
#' @export
check_track.TrackCollection <- function(track){
  if (!inherits(track, "Track")) {
    stop("Only objects of class 'Track' can be added.")
  }
}


#' Add a Track to a TrackCollection
#'
#' This function adds a `Track` object to a `TrackCollection`.
#' 
#' @rdname TrackDataClass
#' @param track_collection An object of class "TrackCollection".
#' @param track An object of class "Track" to add to the collection.
#' @return An updated `TrackCollection` object with the new `Track` added.
#' @export
add_track.TrackCollection <- function(track_collection, track) {
  check_track.TrackCollection(track)
  track_collection$tracks[[length(track_collection$tracks) + 1]] <- track
  return(track_collection)
}

#' Summarize a TrackCollection
#'
#' This function summarizes the content of a `TrackCollection`, including the 
#' number of tracks and the types of tracks.
#' 
#' @rdname TrackDataClass
#' @param track_collection An object of class "TrackCollection".
#' @return A list containing:
#' \describe{
#'   \item{`track_count_summary`}{The number of tracks in the collection.}
#'   \item{`track_type_summary`}{A table summarizing the types of tracks in the collection.}
#' }
#' @method summary TrackCollection
#' @export
summary.TrackCollection <- function(track_collection){
  # Summarize track count, track type
  track_count_summary <- length(track_collection$tracks)
  track_type_summary <- table(sapply(track_collection$tracks, function(track) track$track_type))
  return(list(track_count_summary=track_count_summary, track_type_summary=track_type_summary))
}

#' Print a TrackCollection
#'
#' This function provides a human-readable summary of a `TrackCollection`.
#' @rdname TrackDataClass
#' @param track_collection An object of class "TrackCollection".
#' @return No return value. Prints a summary to the console.
#' @method print TrackCollection
#' @export
print.TrackCollection <- function(track_collection) {
  cat("TrackCollection with", length(track_collection$tracks), "tracks\n")
  summary_ <- summary(track_collection)
  track_type_summary_str <- paste(names(summary_$track_type_summary), summary_$track_type_summary, sep = ": ", collapse = ", ")
  summary_str <- paste(
    "Track Types:\n",
    track_type_summary_str
  )
  cat(summary_str,'\n')
}


make_pairs.TrackCollection <- function(track_collection, force=FALSE) {
  if ('paired_track' %in% names(track_collection)){
    if (!force){
      return(track_collection)
    }
  }
  
  track_collection$paired_track <- '...'
}




# 
# # Create a PairedTrack object (S3 class)
# PairedTrack <- function(start, end) {
#   structure(
#     list(start = start, end = end),
#     class = "PairedTrack"
#   )
# }
# 
# # Print method for PairedTrack
# print.PairedTrack <- function(paired_track) {
#   cat("PairedTrack:\n")
#   cat("Start:\n")
#   print(paired_track$start)
#   cat("End:\n")
#   print(paired_track$end)
# }
# 
# # Create a PairedTrackCollection object (S3 class)
# PairedTrackCollection <- function(track_collection) {
#   paired_tracks <- lapply(track_collection$tracks, function(track) {
#     if (nrow(track$data) >= 2) { # Ensure at least two points
#       start <- track$data[1, ]
#       end <- track$data[nrow(track$data), ]
#       PairedTrack(start, end)
#     } else {
#       NULL
#     }
#   })
#   paired_tracks <- Filter(Negate(is.null), paired_tracks) # Remove NULLs for tracks with less than 2 points
#   
#   structure(
#     list(paired_tracks = paired_tracks),
#     class = "PairedTrackCollection"
#   )
# }
# 
# # Print method for PairedTrackCollection
# print.PairedTrackCollection <- function(paired_collection) {
#   cat("PairedTrackCollection with", length(paired_collection$paired_tracks), "paired tracks:\n")
#   lapply(paired_collection$paired_tracks, print)
# }

# 
# 
# bf <- BirdFlowModels::amewoo
# bf <- add_dynamic_mask(bf)  # To ease transition pain
# dyn_mask <- bf$geom$dynamic_mask
# from_coordinates <- !is.null(x_coord) && !is.null(y_coord)
# 
# # Time
# timesteps <-  lookup_timestep_sequence(bf)
# transitions <- as_transitions(timesteps, bf)
# start <- timesteps[1]
# n=10
# loc <- get_distr(bf, start, from_marginals = FALSE) |> sample_distr(n = n, format = "i", bf = bf)
# row <- i_to_row(loc, bf)
# col <- i_to_col(loc, bf)
# 
# initial_distr <- Matrix::Matrix(0, nrow = n_active(bf), ncol = length(row))
# indices <- rc_to_i(row, col, bf)
# sel <- cbind(indices,  seq_len(length(indices)))
# initial_distr[sel] <- 1
# 
# 
# s <- 1:n_active(bf)
# positions <- apply(dyn_mask, 2, function(x) s[x])
# extract_positions <- function(x, timestep) {
#   # given a dynamically masked distribution generate state space index i
#   
#   pos <- positions[[timestep]]  # positions associated with the d. masked
#   # distribution for this timestep
#   if (is.null(dim(x)))
#     return(pos[as.logical(x)])
#   
#   apply(x, 2, function(vals)  pos[as.logical(vals)])
# }
# 
# distr <- initial_distr[dyn_mask[, start], ]
# 
# 
# trajectory <- matrix(nrow = length(transitions) + 1, ncol = length(row))
# dimnames(trajectory) <-  list(timestep = NULL, route = NULL)
# trajectory[1, ] <- extract_positions(distr, timestep = start)
# 
# distr <- Matrix::Matrix(distr, sparse = TRUE)
# for (i in seq_along(transitions)) {
#   tm <- get_transition(bf,  transitions[i])  # transition matrix
#   distr <- tm %*% distr           # project
#   distr <- sample_distr(distr)  # "one hot"
#   trajectory[i + 1, ] <- extract_positions(distr, timestep = timesteps[i + 1])
# }
# 
# 
# rts <- BirdFlowR:::format_trajectory(trajectory, bf, timesteps)
# 
