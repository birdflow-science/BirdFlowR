
# Object validators ------------------------------------------------------------------
# For the input of Routes class
validate_Routes_input_route_df <- function(route_df){
  for (name in c('route_id', 'date', 'lon', 'lat', 'route_type')){
    if (!name %in% colnames(route_df)){
      stop(sprintf(glue("'{name}' is not found in the input dataframe.")))
    }
  }
  
  if (nrow(route_df)==0){
    stop(sprintf("Input dataframe cannot be empty!"))
  }
  
  validate_Routes_route_id(route_df$route_id)
  validate_Routes_date(route_df$date)
  validate_Routes_lon(route_df$lon)
  validate_Routes_lat(route_df$lat)
  validate_Routes_route_type(route_df$route_type)
}

# For the input of BirdFlowRoutes class
validate_BirdFlowRoutes_input_birdflow_route_df <- function(birdflow_route_df){
  stopifnot(inherits(birdflow_route_df, 'data.frame'))

  # check the features required by direct initiation of BirdFlowRoutes class
  for (name in c('route_id', 'x', 'y', 'i', 'timestep', 'date','route_type')){ # 'stay_id', 'stay_len' is not necessary in this phase
    if (!name %in% colnames(birdflow_route_df)){
      stop(sprintf(glue("'{name}' is not found in the input dataframe.")))
    }
  }
  
  validate_Routes_route_id(birdflow_route_df$route_id)
  validate_BirdFlowRoutes_x(birdflow_route_df$x)
  validate_BirdFlowRoutes_y(birdflow_route_df$y)
  validate_BirdFlowRoutes_i(birdflow_route_df$i)
  validate_BirdFlowRoutes_timestep(birdflow_route_df$i)
  validate_Routes_date(birdflow_route_df$date)
  validate_BirdFlowRoutes_route_type(birdflow_route_df$route_type)
}



# Attributes validators ------------------------------------------------------------------


validate_Routes_route_id <- function(route_id){
  if ((!is.character(route_id) & !is.numeric(route_id)) || any(is.na(route_id))){
    stop(sprintf("'route_id' must be strings or numbers. Missing values are not allowded."))
  }
}

validate_Routes_date <- function(date_vector){
  if (!inherits(date_vector, c("Date", 'POSIXct', 'POSIXlt')) || any(is.na(date_vector))){
    stop("'date' must be of class 'Date', 'POSIXct', or 'POSIXlt'. Missing values are not allowded. Please provide valid Date objects.")
  }
}

validate_Routes_lon <- function(lon_vector){
  if (!is.numeric(lon_vector) || any(is.na(lon_vector))) {
    stop(sprintf("'lon' must be a numeric vector and cannot contain NA values."))
  }
  
  if (max(lon_vector) > 180 || min(lon_vector) > 180){
    stop(sprintf("'lon' not in range (-180, 180)!"))
  }
}

validate_Routes_lat <- function(lat_vector){
  if (!is.numeric(lat_vector) || any(is.na(lat_vector))) {
    stop(sprintf("'lat' must be a numeric vector and cannot contain NA values."))
  }
  
  if (max(lat_vector) > 90 || min(lat_vector) < -90){
    stop(sprintf("'lat' not in range (-90, 90)!"))
  }
}

validate_Routes_route_type <- function(route_type_vector){
  valid_route_types <- c("tracking", "banding",'motus',"unkown")
  if (!all(unique(route_type_vector) %in% valid_route_types)) {
    invalid_types <- unique(route_type_vector)[!unique(route_type_vector) %in% valid_route_types]
    stop(sprintf(
      "Invalid 'route_type' values found: %s. 'route_type' must be one of: %s",
      paste(invalid_types, collapse = ", "),
      paste(valid_route_types, collapse = ", ")
    ))
  }
}

validate_BirdFlowRoutes_route_type <- function(route_type_vector){
  valid_route_types <- c("tracking", "banding",'motus','synthetic',"unkown")
  if (!all(unique(route_type_vector) %in% valid_route_types)) {
    invalid_types <- unique(route_type_vector)[!unique(route_type_vector) %in% valid_route_types]
    stop(sprintf(
      "Invalid 'route_type' values found: %s. 'route_type' must be one of: %s",
      paste(invalid_types, collapse = ", "),
      paste(valid_route_types, collapse = ", ")
    ))
  }
}

validate_BirdFlowRoutes_x <- function(x_vector){
  if (!is.numeric(x_vector) || any(is.na(x_vector))) {
    stop(sprintf("'x' must be a numeric vector and cannot contain NA values."))
  }
}

validate_BirdFlowRoutes_y <- function(y_vector){
  if (!is.numeric(y_vector) || any(is.na(y_vector))) {
    stop(sprintf("'y' must be a numeric vector and cannot contain NA values."))
  }
}

validate_BirdFlowRoutes_i <- function(i_vector){
  if (!is.integer(i_vector) || any(is.na(i_vector))) {
    stop(sprintf("'i' must be an integer vector and cannot contain NA values."))
  }
}

validate_BirdFlowRoutes_timestep <- function(timestep_vector){
  if (!is.integer(timestep_vector) || any(is.na(timestep_vector))) {
    stop(sprintf("timestep' must be an integer vector and cannot contain NA values."))
  }
}

validate_BirdFlowRoutes_stay_id <- function(stay_id_vector){
  if (!(is.numeric(stay_id_vector) || is.character(stay_id_vector)) || any(is.na(stay_id_vector))) {
    stop(sprintf("stay_id' must be a numeric or charactor vector and cannot contain NA values."))
  }
}

validate_BirdFlowRoutes_stay_len <- function(stay_len_vector){
  if (!is.integer(stay_len_vector) || any(is.na(stay_len_vector))) {
    stop(sprintf("stay_len' must be an integer vector and cannot contain NA values."))
  }
}





