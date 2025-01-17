#' @name object_validators
#' @title Object Validators
#' @description A collection of functions to validate input data frames for `Routes`, `BirdFlowRoutes`, 
#' and `BirdFlowIntervals`. These validators ensure that the data frames contain all required columns, 
#' conform to expected data types, and adhere to additional constraints specific to each object class.
#'
#' @details These functions perform comprehensive checks to ensure data integrity. They verify that:
#' - All required columns are present.
#' - Data types match expected formats (e.g., numeric, character, Date).
#' - No missing or invalid values are present in critical fields.
#' - Additional constraints specific to the class, such as unique timesteps for routes, are satisfied.
#'
#' ### Functions Included:
#' - `validate_Routes_route_df()`: Validates the input data frame for the `Routes` class.
#' - `validate_BirdFlowRoutes_birdflow_route_df()`: Validates the input data frame for the `BirdFlowRoutes` class.
#' - `validate_BirdFlowIntervals_birdflow_intervals()`: Validates the input data frame for the `BirdFlowIntervals` class.
#'
#' @param route_df A data frame containing data for the `Routes` class. It must include columns like `route_id`, `date`, `lon`, `lat`, and `route_type`.
#' @param birdflow_route_df A data frame containing data for the `BirdFlowRoutes` class. It must include additional columns such as `x`, `y`, `i`, and `timestep`.
#' @param birdflow_interval_df A data frame containing data for the `BirdFlowIntervals` class. It must include columns such as `lon1`, `lon2`, `y1`, `y2`, `i1`, `i2`, `timestep1`, `timestep2`.
#' @return These functions return nothing if validation succeeds. If validation fails, an error message is raised detailing the issue.
#'
#' @examples
#' # Example: Validating a Routes data frame
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' validate_Routes_route_df(route_df)
#'
#' # Example: Validating a BirdFlowRoutes data frame
#' birdflow_route_df <- data.frame(
#'   route_id = 1:3,
#'   x = c(1000, 2000, 3000),
#'   y = c(500, 600, 700),
#'   i = as.integer(c(1, 2, 3)),
#'   timestep = as.integer(c(1, 1, 2)),
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' validate_BirdFlowRoutes_birdflow_route_df(birdflow_route_df)
#'
#' @seealso
#' - [Attribute Validators](?attribute_validators)
#' - [Column Targeting Functions](?target_columns)
NULL


# Object validators ------------------------------------------------------------------
# For the input of Routes class

#' @rdname object_validators
validate_Routes_route_df <- function(route_df){
  for (name in get_target_columns_Routes(type='input')){
    if (!name %in% colnames(route_df)){
      stop(sprintf(glue::glue("'{name}' is not found in the input dataframe.")))
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


#' @rdname object_validators
validate_BirdFlowRoutes_birdflow_route_df <- function(birdflow_route_df){
  # For the input of BirdFlowRoutes class
  stopifnot(inherits(birdflow_route_df, 'data.frame'))

  # check the features required by direct initiation of BirdFlowRoutes class
  for (name in get_target_columns_BirdFlowRoutes(type='input')){
    if (!name %in% colnames(birdflow_route_df)){
      stop(sprintf(glue::glue("'{name}' is not found in the input dataframe.")))
    }
  }
  
  validate_Routes_route_id(birdflow_route_df$route_id)
  validate_BirdFlowRoutes_x(birdflow_route_df$x)
  validate_BirdFlowRoutes_y(birdflow_route_df$y)
  validate_BirdFlowRoutes_i(birdflow_route_df$i)
  validate_BirdFlowRoutes_timestep(birdflow_route_df$route_id, birdflow_route_df$timestep) # Should not have duplicated timesteps within each route! Should select only one data point for each timestep for each route. 
  validate_Routes_date(birdflow_route_df$date)
  validate_BirdFlowRoutes_route_type(birdflow_route_df$route_type)
}

#' @rdname object_validators
validate_BirdFlowIntervals_birdflow_intervals <- function(birdflow_interval_df){
  stopifnot(inherits(birdflow_interval_df, 'data.frame'))
  # check the features required by direct initiation of BirdFlowIntervals class
  for (name in get_target_columns_BirdFlowIntervals(type='input')){
    if (!name %in% colnames(birdflow_interval_df)){
      stop(sprintf(glue::glue("'{name}' is not found in the input dataframe.")))
    }
  }
}

#' @name target_columns
#' @title Get Target Columns for Data Frames
#' @description A collection of internal utility functions to retrieve the required column names 
#' for `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals` objects. These functions ensure 
#' consistency in data frame structures across different processing steps.
#'
#' @details These functions return the expected column names for `Routes`, `BirdFlowRoutes` and `BirdFlowIntervals` dataclasses. 
#' Columns may vary depending on whether the context is for input or output processing:
#' - **Input Columns**: Columns required for validation or initial data ingestion.
#' - **Output Columns**: Columns expected after processing or transformation.
#'
#' ### Functions Included:
#' - `get_target_columns_Routes()`: Returns column names for `Routes` objects.
#' - `get_target_columns_BirdFlowRoutes()`: Returns column names for `BirdFlowRoutes` objects.
#' - `get_target_columns_BirdFlowIntervals()`: Returns column names for `BirdFlowIntervals` objects.
#'
#' @param type A character string specifying the context for the columns. Either `'input'` (default) 
#' for required input columns or `'output'` for columns expected after processing.
#' @return A character vector containing the expected column names.
#'
#' @examples
#' # Get input columns for Routes
#' get_target_columns_Routes(type = "input")
#'
#' # Get output columns for BirdFlowRoutes
#' get_target_columns_BirdFlowRoutes(type = "output")
#'
#' # Get input columns for BirdFlowIntervals
#' get_target_columns_BirdFlowIntervals(type = "input")
#'
#' @seealso
#' - [Object Validators](?object_validators)
#' - [Attribute Validators](?attribute_validators)
NULL

#' @rdname target_columns
get_target_columns_Routes <- function(type='input'){
  if (type=='input'){
    return(c('route_id', 'date', 'lon', 'lat', 'route_type'))
  } else if (type=='output') {
    return(c('route_id', 'date', 'lon', 'lat', 'route_type')) # No additional columns added when making this class
  } else {
    stop(glue::glue("The type should be 'input' or 'output', got {type}"))
  }
}

#' @rdname target_columns
get_target_columns_BirdFlowRoutes <- function(type='input'){
  if (type=='input'){
    return(c('route_id', 'x', 'y', 'i', 'timestep', 'date', 'route_type'))  # 'stay_id', 'stay_len' is not necessary in this phase
  } else if (type=='output') {
    return(c('route_id', 'x', 'y', 'i', 'timestep', 'date', 'route_type', 'stay_id', 'stay_len')) # 'stay_id', 'stay_len'
  } else {
    stop(glue::glue("The type should be 'input' or 'output', got {type}"))
  }
}

#' @rdname target_columns
get_target_columns_BirdFlowIntervals <- function(type='input'){
  if (type=='input'){
    return(c("interval_id", "route_id", "lon1", "lon2", "lat1", "lat2", "x1", "x2", "y1", "y2", "i1", "i2", "date1", "date2", "timestep1", "timestep2", 'route_type')) 
  } else if (type=='output') {
    return(c("interval_id", "route_id", "lon1", "lon2", "lat1", "lat2", "x1", "x2", "y1", "y2", "i1", "i2", "date1", "date2", "timestep1", "timestep2", 'route_type')) # 
  } else {
    stop(glue::glue("The type should be 'input' or 'output', got {type}"))
  }
}


# Attributes validators ------------------------------------------------------------------

#' @name attribute_validators
#' @title Attribute Validators
#' @description A collection of internal utility functions to validate individual attributes 
#' within `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals`, and related objects. These functions ensure the correctness, 
#' consistency, and validity of attributes used in BirdFlow data structures.
#'
#' @details These functions validate specific attributes or columns in input data frames. They check:
#' - **General Attributes**:
#'   - `route_id`: Ensures IDs are valid and non-missing.
#'   - `date`: Ensures date formats and non-missing values.
#'   - `lon` and `lat`: Ensure longitude and latitude values are within valid ranges.
#'   - `route_type`: Ensures only valid route types are present.
#' - **Spatial Attributes** (for `BirdFlowRoutes`):
#'   - `x` and `y`: Ensure numeric spatial coordinates.
#'   - `i`: Ensures valid spatial indices as integers.
#'   - `timestep`: Ensures unique timesteps per `route_id`.
#'   - `stay_id` and `stay_len`: Ensure valid stay identifiers and lengths.
#' - **Additional Attributes**:
#'   - `species`: Ensures species data contains required components.
#'   - `geom`: Ensures geometry data includes all required fields.
#'   - `dates`: Validates date-related data frames, checking required columns.
#'
#' These functions are intended for internal use and are not exported for user-facing functionality.
#'
#' ### Functions Included:
#' - General Validators:
#'   - `validate_Routes_route_id()`
#'   - `validate_Routes_date()`
#'   - `validate_Routes_lon()`, `validate_Routes_lat()`
#'   - `validate_Routes_route_type()`
#' - BirdFlowRoutes Validators:
#'   - `validate_BirdFlowRoutes_x()`, `validate_BirdFlowRoutes_y()`
#'   - `validate_BirdFlowRoutes_i()`, `validate_BirdFlowRoutes_timestep()`
#'   - `validate_BirdFlowRoutes_stay_id()`, `validate_BirdFlowRoutes_stay_len()`
#'   - `validate_BirdFlowRoutes_species()`
#' - Geometry and Dates:
#'   - `validate_BirdFlowRoutes_geom()`
#'   - `validate_BirdFlowRoutes_dates()`
#'
#' @param route_id A vector of route IDs (character or numeric). Must not contain missing values.
#' @param date_vector A vector of dates (`Date`, `POSIXct`, or `POSIXlt`). Must not contain missing values.
#' @param lon_vector, lat_vector Numeric vectors for longitude and latitude. Must not contain missing values and must be within valid ranges.
#' @param route_type_vector A character vector of route types. Must only contain valid types.
#' @param x_vector, y_vector Numeric vectors for spatial coordinates. Must not contain missing values.
#' @param i_vector An integer vector for spatial indices. Must not contain missing values.
#' @param timestep_vector An integer vector for timesteps, paired with `route_id_vector`. Must not have duplicates within a `route_id`.
#' @param stay_id_vector A numeric or character vector for stay IDs. Must not contain missing values.
#' @param stay_len_vector An integer vector for stay lengths. Must not contain missing values.
#' @param species A list with species information. Must include `species_code`, `scientific_name`, and `common_name`.
#' @param geom A list containing geometry attributes. Must include `nrow`, `ncol`, `res`, `ext`, `crs`, `mask`, and `dynamic_mask`.
#' @param dates A data frame with date-related information. Must include `interval`, `date`, `midpoint`, `start`, `end`, `doy`, and `week`.
#'
#' @return Each function returns `TRUE` if validation succeeds. If validation fails, an error is raised with details about the issue.
#'
#' @examples
#' # Validate route IDs
#' validate_Routes_route_id(c(1, 2, 3))
#'
#' # Validate dates
#' validate_Routes_date(as.Date(c("2024-01-01", "2024-01-02")))
#'
#' # Validate longitude and latitude
#' validate_Routes_lon(c(-90, -89, -88))
#' validate_Routes_lat(c(40, 41, 42))
#'
#' # Validate spatial indices
#' validate_BirdFlowRoutes_x(c(1000, 2000, 3000))
#' validate_BirdFlowRoutes_y(c(500, 600, 700))
#' validate_BirdFlowRoutes_i(as.integer(c(1, 2, 3)))
#'
#' # Validate timesteps
#' route_ids <- c("route1", "route1", "route2")
#' timesteps <- as.integer(c(1, 2, 1))
#' validate_BirdFlowRoutes_timestep(route_ids, timesteps)
#'
#' # Validate species
#' species <- list(
#'   species_code = "SP001",
#'   scientific_name = "Hirundo rustica",
#'   common_name = "Barn Swallow"
#' )
#' validate_BirdFlowRoutes_species(species)
#'
#' # Validate geometry
#' geom <- list(
#'   nrow = 100, ncol = 200, res = 1, ext = NULL, crs = NULL, mask = NULL, dynamic_mask = NULL
#' )
#' validate_BirdFlowRoutes_geom(geom)
#'
#' # Validate dates
#' dates <- data.frame(
#'   interval = 1:3,
#'   date = as.Date("2024-01-01") + 0:2,
#'   midpoint = as.Date("2024-01-01") + 0:2,
#'   start = as.Date("2024-01-01") + 0:2,
#'   end = as.Date("2024-01-03") + 0:2,
#'   doy = 1:3,
#'   week = 1:3
#' )
#' validate_BirdFlowRoutes_dates(dates)
#'
#' @seealso
#' - [Object Validators](?object_validators)
#' - [Column Targeting Functions](?target_columns)
#' @keywords internal
NULL

#' @rdname attribute_validators
validate_Routes_route_id <- function(route_id){
  if ((!is.character(route_id) & !is.numeric(route_id)) || any(is.na(route_id))){
    stop(sprintf("'route_id' must be strings or numbers. Missing values are not allowded."))
  }
}

#' @rdname attribute_validators
validate_Routes_date <- function(date_vector){
  if (!inherits(date_vector, c("Date", 'POSIXct', 'POSIXlt')) || any(is.na(date_vector))){
    stop("'date' must be of class 'Date', 'POSIXct', or 'POSIXlt'. Missing values are not allowded. Please provide valid Date objects.")
  }
}

#' @rdname attribute_validators
validate_Routes_lon <- function(lon_vector){
  if (!is.numeric(lon_vector) || any(is.na(lon_vector))) {
    stop(sprintf("'lon' must be a numeric vector and cannot contain NA values."))
  }
  
  if (max(lon_vector) > 180 || min(lon_vector) > 180){
    stop(sprintf("'lon' not in range (-180, 180)!"))
  }
}

#' @rdname attribute_validators
validate_Routes_lat <- function(lat_vector){
  if (!is.numeric(lat_vector) || any(is.na(lat_vector))) {
    stop(sprintf("'lat' must be a numeric vector and cannot contain NA values."))
  }
  
  if (max(lat_vector) > 90 || min(lat_vector) < -90){
    stop(sprintf("'lat' not in range (-90, 90)!"))
  }
}

#' @rdname attribute_validators
validate_Routes_route_type <- function(route_type_vector){
  valid_route_types <- c("tracking", "banding",'motus',"unknown")
  if (!all(unique(route_type_vector) %in% valid_route_types)) {
    invalid_types <- unique(route_type_vector)[!unique(route_type_vector) %in% valid_route_types]
    stop(sprintf(
      "Invalid 'route_type' values found: %s. 'route_type' must be one of: %s",
      paste(invalid_types, collapse = ", "),
      paste(valid_route_types, collapse = ", ")
    ))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_route_type <- function(route_type_vector){
  valid_route_types <- c("tracking", "banding",'motus','synthetic',"unknown")
  if (!all(unique(route_type_vector) %in% valid_route_types)) {
    invalid_types <- unique(route_type_vector)[!unique(route_type_vector) %in% valid_route_types]
    stop(sprintf(
      "Invalid 'route_type' values found: %s. 'route_type' must be one of: %s",
      paste(invalid_types, collapse = ", "),
      paste(valid_route_types, collapse = ", ")
    ))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_x <- function(x_vector){
  if (!is.numeric(x_vector) || any(is.na(x_vector))) {
    stop(sprintf("'x' must be a numeric vector and cannot contain NA values."))
  }
}


#' @rdname attribute_validators
validate_BirdFlowRoutes_y <- function(y_vector){
  if (!is.numeric(y_vector) || any(is.na(y_vector))) {
    stop(sprintf("'y' must be a numeric vector and cannot contain NA values."))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_i <- function(i_vector){
  if (!is.integer(i_vector) || any(is.na(i_vector))) {
    stop(sprintf("'i' must be an integer vector and cannot contain NA values."))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_timestep <- function(route_id_vector, timestep_vector){
  if (!is.integer(timestep_vector) || any(is.na(timestep_vector))) {
    stop(sprintf("timestep' must be an integer vector and cannot contain NA values."))
  }
  
  tmp_new_df <- data.frame(list(route_id=route_id_vector, timestep=timestep_vector))
  duplicated_timestep_check <- tmp_new_df |> 
    dplyr::group_by(route_id) |> 
    dplyr::summarize(
    duplicated_timesteps = any(duplicated(timestep)),
    .groups = "drop"
  )
  
  if (any(duplicated_timestep_check$duplicated_timesteps)) {
    stop("Duplicated timesteps detected within one or more route IDs. Should select only one data point per timestep for each route.")
  }
  # Should not have duplicated timesteps within each route! Should select only one data point per timestep for each route. 
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_stay_id <- function(stay_id_vector){
  if (!(is.numeric(stay_id_vector) || is.character(stay_id_vector)) || any(is.na(stay_id_vector))) {
    stop(sprintf("stay_id' must be a numeric or charactor vector and cannot contain NA values."))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_stay_len <- function(stay_len_vector){
  if (!is.integer(stay_len_vector) || any(is.na(stay_len_vector))) {
    stop(sprintf("stay_len' must be an integer vector and cannot contain NA values."))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_species <- function(species){
  exists_names <- names(species)
  target_name_list <- c("species_code", "scientific_name", "common_name")
  for (name in target_name_list){
    if (!(name %in% exists_names)){
      stop(sprintf(glue::glue('{name} component not found in species!')))
    }
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_geom <- function(geom){
  exists_names <- names(geom)
  target_name_list <- c("nrow","ncol","res","ext","crs","mask","dynamic_mask")
  for (name in target_name_list){
    if (!(name %in% exists_names)){
      stop(sprintf(glue::glue('{name} component not found in geom!')))
    }
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_dates <- function(dates){
  stopifnot(inherits(dates, 'data.frame'))
  exists_names <- colnames(dates)
  target_name_list <- c("interval", "date", "midpoint", "start", "end", "doy", "week")
  for (name in target_name_list){
    if (!(name %in% exists_names)){
      stop(sprintf(glue::glue('{name} component not found in dates!')))
    }
  }
}


