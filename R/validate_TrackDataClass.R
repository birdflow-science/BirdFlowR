
# Object validators ------------------------------------------------------------------
# For the input of Tracks class
validate_Tracks_input_track_df <- function(track_df){
  for (name in c('track_id', 'date', 'lon', 'lat', 'track_type')){
    if (!name %in% colnames(track_df)){
      stop(sprintf(glue("'{name}' is not found in the input dataframe.")))
    }
  }
  
  if (nrow(track_df)==0){
    stop(sprintf("Input dataframe cannot be empty!"))
  }
  
  validate_Tracks_track_id(track_df$track_id)
  validate_Tracks_date(track_df$date)
  validate_Tracks_lon(track_df$lon)
  validate_Tracks_lat(track_df$lat)
  validate_Tracks_track_type(track_df$track_type)
}

# For the input of BirdFlowTracks class
validate_BirdFlowTracks_input_birdflow_track_df <- function(birdflow_track_df){
  stopifnot(inherits(birdflow_track_df, 'data.frame'))

  # check the features required by direct initiation of BirdFlowTracks class
  for (name in c('track_id', 'x', 'y', 'i', 'timestep', 'date','track_type')){ # 'stay_id', 'stay_len' is not necessary in this phase
    if (!name %in% colnames(birdflow_track_df)){
      stop(sprintf(glue("'{name}' is not found in the input dataframe.")))
    }
  }
  
  validate_Tracks_track_id(birdflow_track_df$track_id)
  validate_BirdFlowTracks_x(birdflow_track_df$x)
  validate_BirdFlowTracks_y(birdflow_track_df$y)
  validate_BirdFlowTracks_i(birdflow_track_df$i)
  validate_BirdFlowTracks_timestep(birdflow_track_df$i)
  validate_Tracks_date(birdflow_track_df$date)
  validate_BirdFlowTracks_track_type(birdflow_track_df$track_type)
}



# Attributes validators ------------------------------------------------------------------


validate_Tracks_track_id <- function(track_id){
  if ((!is.character(track_id) & !is.numeric(track_id)) || any(is.na(track_id))){
    stop(sprintf("'track_id' must be strings or numbers. Missing values are not allowded."))
  }
}

validate_Tracks_date <- function(date_vector){
  if (!inherits(date_vector, c("Date", 'POSIXct', 'POSIXlt')) || any(is.na(date_vector))){
    stop("'date' must be of class 'Date', 'POSIXct', or 'POSIXlt'. Missing values are not allowded. Please provide valid Date objects.")
  }
}

validate_Tracks_lon <- function(lon_vector){
  if (!is.numeric(lon_vector) || any(is.na(lon_vector))) {
    stop(sprintf("'lon' must be a numeric vector and cannot contain NA values."))
  }
  
  if (max(lon_vector) > 180 || min(lon_vector) > 180){
    stop(sprintf("'lon' not in range (-180, 180)!"))
  }
}

validate_Tracks_lat <- function(lat_vector){
  if (!is.numeric(lat_vector) || any(is.na(lat_vector))) {
    stop(sprintf("'lat' must be a numeric vector and cannot contain NA values."))
  }
  
  if (max(lat_vector) > 90 || min(lat_vector) < -90){
    stop(sprintf("'lat' not in range (-90, 90)!"))
  }
}

validate_Tracks_track_type <- function(track_type_vector){
  valid_track_types <- c("tracking", "banding",'motus',"unkown")
  if (!all(unique(track_type_vector) %in% valid_track_types)) {
    invalid_types <- unique(track_type_vector)[!unique(track_type_vector) %in% valid_track_types]
    stop(sprintf(
      "Invalid 'track_type' values found: %s. 'track_type' must be one of: %s",
      paste(invalid_types, collapse = ", "),
      paste(valid_track_types, collapse = ", ")
    ))
  }
}

validate_BirdFlowTracks_track_type <- function(track_type_vector){
  valid_track_types <- c("tracking", "banding",'motus','synthetic',"unkown")
  if (!all(unique(track_type_vector) %in% valid_track_types)) {
    invalid_types <- unique(track_type_vector)[!unique(track_type_vector) %in% valid_track_types]
    stop(sprintf(
      "Invalid 'track_type' values found: %s. 'track_type' must be one of: %s",
      paste(invalid_types, collapse = ", "),
      paste(valid_track_types, collapse = ", ")
    ))
  }
}

validate_BirdFlowTracks_x <- function(x_vector){
  if (!is.numeric(x_vector) || any(is.na(x_vector))) {
    stop(sprintf("'x' must be a numeric vector and cannot contain NA values."))
  }
}

validate_BirdFlowTracks_y <- function(y_vector){
  if (!is.numeric(y_vector) || any(is.na(y_vector))) {
    stop(sprintf("'y' must be a numeric vector and cannot contain NA values."))
  }
}

validate_BirdFlowTracks_i <- function(i_vector){
  if (!is.integer(i_vector) || any(is.na(i_vector))) {
    stop(sprintf("'i' must be an integer vector and cannot contain NA values."))
  }
}

validate_BirdFlowTracks_timestep <- function(timestep_vector){
  if (!is.integer(timestep_vector) || any(is.na(timestep_vector))) {
    stop(sprintf("timestep' must be an integer vector and cannot contain NA values."))
  }
}

validate_BirdFlowTracks_stay_id <- function(stay_id_vector){
  if (!(is.numeric(stay_id_vector) || is.character(stay_id_vector)) || any(is.na(stay_id_vector))) {
    stop(sprintf("stay_id' must be a numeric or charactor vector and cannot contain NA values."))
  }
}

validate_BirdFlowTracks_stay_len <- function(stay_len_vector){
  if (!is.integer(stay_len_vector) || any(is.na(stay_len_vector))) {
    stop(sprintf("stay_len' must be an integer vector and cannot contain NA values."))
  }
}





