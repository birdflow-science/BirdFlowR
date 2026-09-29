#' @name object_validators
#' @title Object Validators
#' @description A collection of functions to validate input data frames for
#' `Routes`, `BirdFlowRoutes`,
#' and `BirdFlowIntervals`. These validators ensure that the data frames
#' contain all required columns,
#' conform to expected data types, and adhere to additional constraints
#' specific to each object class.
#'
#' @details These functions perform comprehensive checks to ensure data
#' integrity. They verify that:
#' - All required columns are present.
#' - Data types match expected formats (e.g., numeric, character, Date).
#' - No missing or invalid values are present in critical fields.
#' - Additional constraints specific to the class, such as unique timesteps
#' for routes, are satisfied.
#'
#' ### Functions Included:
#' - `validate_route_df()`: Validates the input data frame for the `Routes`
#' or `BirdFlowRoutes` classes, depending on `class`.
#' - `validate_interval_df()`: Validates the input data frame for the
#' `BirdFlowIntervals` class.
#'
#' @param df A data frame containing data for the `Routes` or `BirdFlowRoutes`
#' class (`validate_route_df()`) or the `BirdFlowIntervals` class
#' (`validate_interval_df()`).
#' @param class Either `"Routes"` or `"BirdFlowRoutes"`; selects which set of
#' columns and checks `validate_route_df()` applies.
#' @param routes A `Routes` object.
#' @param birdflow_routes A `BirdFlowRoutes` object.
#' @param birdflow_intervals A `BirdFlowIntervals` object.
#' @return These functions return nothing if validation succeeds.
#' If validation fails, an error message is raised detailing the issue.
#'
#' @seealso
#' - [Attribute Validators](?attribute_validators)
#' - [Column Targeting Functions](?target_columns)
NULL


# Object validators --------------------------------------------------------

#' @rdname object_validators
validate_route_df <- function(df, class = "Routes") {
  stopifnot(class %in% c("Routes", "BirdFlowRoutes"))
  stopifnot(inherits(df, "data.frame"))

  target_columns <- if (class == "BirdFlowRoutes") {
    get_target_columns_BirdFlowRoutes(type = "input")
  } else {
    get_target_columns_Routes(type = "input")
  }
  for (name in target_columns) {
    if (!name %in% colnames(df)) {
      stop(sprintf("'%s' is not found in the input dataframe.", name))
    }
  }

  if (nrow(df) == 0) {
    stop("Input dataframe cannot be empty!")
  }

  # Columns shared by Routes and BirdFlowRoutes
  validate_Routes_route_id(df$route_id)
  validate_Routes_lon(df$lon)
  validate_Routes_lat(df$lat)
  validate_Routes_route_type(df$route_type)

  if (class == "BirdFlowRoutes") {
    validate_BirdFlowRoutes_x(df$x)
    validate_BirdFlowRoutes_y(df$y)
    validate_BirdFlowRoutes_i(df$i)
    validate_BirdFlowRoutes_timestep(df$timestep)
    # Also checks for duplicated dates within a route_id, which is required
    # for BirdFlowRoutes but not Routes.
    validate_BirdFlowRoutes_date(df$route_id, df$date)
  } else {
    validate_Routes_date(df$date)
  }
}

#' @rdname object_validators
validate_interval_df <- function(df) {
  stopifnot(inherits(df, "data.frame"))

  target_columns <- get_target_columns_BirdFlowIntervals(type = "input")
  for (name in target_columns) {
    if (!name %in% colnames(df)) {
      stop(sprintf("'%s' is not found in the input dataframe.", name))
    }
  }

  if (nrow(df) == 0) {
    stop("Input dataframe cannot be empty!")
  }

  validate_BirdFlowIntervals_interval_id(df$interval_id)
  validate_Routes_route_id(df$route_id)
  validate_BirdFlowRoutes_x(df$x1)
  validate_BirdFlowRoutes_x(df$x2)
  validate_BirdFlowRoutes_y(df$y1)
  validate_BirdFlowRoutes_y(df$y2)
  validate_BirdFlowRoutes_i(df$i1)
  validate_BirdFlowRoutes_i(df$i2)
  validate_Routes_lon(df$lon1)
  validate_Routes_lon(df$lon2)
  validate_Routes_lat(df$lat1)
  validate_Routes_lat(df$lat2)
  validate_Routes_date(df$date1)
  validate_Routes_date(df$date2)
  validate_BirdFlowRoutes_timestep(df$timestep1)
  validate_BirdFlowRoutes_timestep(df$timestep2)
  validate_Routes_route_type(df$route_type)
}


#' @rdname object_validators
#' @export
validate_routes <- function(routes) {
  stopifnot(inherits(routes, "Routes"))

  for (name in c("data", "species", "source")) {
    if (!name %in% names(routes)) {
      stop(sprintf("'%s' is not found in the input object!", name))
    }
  }

  validate_route_df(routes$data, class = "Routes")
}

#' @rdname object_validators
#' @export
validate_Routes <- function(routes) {
  warning("validate_Routes() is deprecated. ",
          "Please use validate_routes() instead.")
  validate_routes(routes)
}


#' @rdname object_validators
#' @export
validate_birdflow_routes <- function(birdflow_routes) {
  stopifnot(inherits(birdflow_routes, "BirdFlowRoutes"))
  stopifnot(inherits(birdflow_routes, "Routes"))

  for (name in c("data", "species", "metadata", "geom", "dates", "source")) {
    if (!name %in% names(birdflow_routes)) {
      stop(sprintf("'%s' is not found in the input object!", name))
    }
  }

  # validate elements
  validate_BirdFlowRoutes_species(birdflow_routes$species)
  validate_BirdFlowRoutes_metadata(birdflow_routes$metadata)
  validate_geom(birdflow_routes$geom,
                n_active = birdflow_routes$metadata$n_active)
  validate_BirdFlowRoutes_dates(birdflow_routes$dates)

  validate_route_df(birdflow_routes$data, class = "BirdFlowRoutes")
}

#' @rdname object_validators
#' @export
validate_BirdFlowRoutes <- function(birdflow_routes) {
  warning("validate_BirdFlowRoutes() is deprecated. ",
          "Please use validate_birdflow_routes() instead.")
  validate_birdflow_routes(birdflow_routes)
}


#' @rdname object_validators
#' @export
validate_birdflow_intervals <- function(birdflow_intervals) {
  stopifnot(inherits(birdflow_intervals, "BirdFlowIntervals"))

  for (name in c("data", "species", "metadata", "geom", "dates", "source")) {
    if (!name %in% names(birdflow_intervals)) {
      stop(sprintf("'%s' is not found in the input object!", name))
    }
  }

  # validate elements
  validate_BirdFlowRoutes_species(birdflow_intervals$species)
  validate_BirdFlowRoutes_metadata(birdflow_intervals$metadata)
  validate_geom(birdflow_intervals$geom,
                n_active = birdflow_intervals$metadata$n_active)
  validate_BirdFlowRoutes_dates(birdflow_intervals$dates)

  validate_interval_df(birdflow_intervals$data)
}

#' @rdname object_validators
#' @export
validate_BirdFlowIntervals <- function(birdflow_intervals) {
  warning("validate_BirdFlowIntervals() is deprecated. ",
          "Please use validate_birdflow_intervals() instead.")
  validate_birdflow_intervals(birdflow_intervals)
}




#' @name target_columns
#' @title Get Target Columns for Data Frames
#' @description A collection of internal utility functions to retrieve the
#' required column names
#' for `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals` objects. These
#' functions ensure
#' consistency in data frame structures across different processing steps.
#'
#' @details These functions return the expected column names for `Routes`,
#' `BirdFlowRoutes` and `BirdFlowIntervals` data classes.
#' Columns may vary depending on whether the context is for input or output
#' processing:
#' - **Input Columns**: Columns required for validation or initial data
#' ingestion.
#' - **Output Columns**: Columns expected after processing or transformation.
#'
#' ### Functions Included:
#' - `get_target_columns_Routes()`: Returns column names for `Routes` objects.
#' - `get_target_columns_BirdFlowRoutes()`: Returns column names for
#' `BirdFlowRoutes` objects.
#' - `get_target_columns_BirdFlowIntervals()`: Returns column names for
#' `BirdFlowIntervals` objects.
#'
#' @param type A character string specifying the context for the columns.
#' Either `'input'` (default)
#' for required input columns or `'output'` for columns expected after
#' processing.
#' @return A character vector containing the expected column names.
#'
#' @seealso
#' - [Object Validators](?object_validators)
#' - [Attribute Validators](?attribute_validators)
NULL

#' @rdname target_columns
get_target_columns_Routes <- function(type = "input") {
  if (type == "input") {
    return(c("route_id", "date", "lon", "lat", "route_type"))
  } else if (type == "output") {
    return(c("route_id", "date", "lon", "lat", "route_type"))
    # No additional columns added when making this class
  } else {
    stop(sprintf("The type should be 'input' or 'output', got %s.", type))
  }
}

#' @rdname target_columns
get_target_columns_BirdFlowRoutes <- function(type = "input") {
  if (type == "input") {
    return(
      c(
        "route_id", "x", "y", "i", "lon", "lat",
        "timestep", "date", "route_type"
      )
    ) # 'stay_id', 'stay_len' is not necessary in this phase
  } else if (type == "output") {
    return(c(
      "route_id", "x", "y", "i", "lon", "lat", "timestep", "date",
      "route_type", "stay_id", "stay_len"
    )) # 'stay_id', 'stay_len'
  } else {
    stop(sprintf("The type should be 'input' or 'output', got %s.", type))
  }
}

#' @rdname target_columns
get_target_columns_BirdFlowIntervals <- function(type = "input") {
  if (type == "input") {
    return(c(
      "interval_id", "route_id", "x1", "x2", "y1", "y2", "i1", "i2",
      "lon1", "lon2", "lat1", "lat2", "date1", "date2", "timestep1",
      "timestep2", "route_type"
    ))
  } else if (type == "output") {
    return(c(
      "interval_id", "route_id", "x1", "x2", "y1", "y2", "i1", "i2",
      "lon1", "lon2", "lat1", "lat2", "date1", "date2",
      "timestep1", "timestep2", "route_type"
    )) #
  } else {
    stop(sprintf("The type should be 'input' or 'output', got %s.", type))
  }
}


# Attributes validators ---------------------------------------------------

#' @name attribute_validators
#' @title Attribute Validators
#' @description A collection of internal utility functions to validate
#' individual attributes
#' within `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals`, and related
#' objects. These functions ensure the correctness,
#' consistency, and validity of attributes used in BirdFlow data structures.
#'
#' @details These functions validate specific attributes or columns in input
#' data frames. They check:
#' - **General Attributes**:
#'   - `route_id`: Ensures IDs are valid and non-missing.
#'   - `date`: Ensures date formats and non-missing values.
#'   - `lon` and `lat`: Ensure longitude and latitude values are within
#'   valid ranges.
#'   - `route_type`: Warns when non-standard route types are present.
#' - **Spatial Attributes** (for `BirdFlowRoutes`):
#'   - `x` and `y`: Ensure numeric spatial coordinates.
#'   - `i`: Ensures valid spatial indices as integers.
#'   - `timestep`: Ensures unique timesteps per `route_id`.
#' - **Additional Attributes**:
#'   - `species`: Ensures species data contains required components.
#'   - `geom`: Ensures geometry data includes all required fields.
#'   - `dates`: Validates date-related data frames, checking required columns.
#'
#' These functions are intended for internal use and are not exported for
#' user-facing functionality.
#'
#' ### Functions Included:
#' - General Validators:
#'   - `validate_Routes_route_id()`
#'   - `validate_Routes_date()`
#'   - `validate_Routes_lon()`, `validate_Routes_lat()`
#'   - `validate_Routes_route_type()`
#' - `BirdFlowRoutes` Validators:
#'   - `validate_BirdFlowRoutes_x()`, `validate_BirdFlowRoutes_y()`
#'   - `validate_BirdFlowRoutes_i()`, `validate_BirdFlowRoutes_timestep()`,
#'   - `validate_BirdFlowRoutes_date()`,
#'   - `validate_BirdFlowRoutes_species()`
#' - `BirdFlowIntervals` Validators:
#'   - `validate_BirdFlowIntervals_interval_id()`
#' - Dates and Metadata:
#'   - `validate_BirdFlowRoutes_dates()`
#'   - `validate_BirdFlowRoutes_metadata()`
#'
#' @param route_id A vector of route IDs (character or numeric). Must not
#' contain missing values.
#' @param date_vector A vector of dates (`Date`, `POSIXct`, or `POSIXlt`).
#' Must not contain missing values;
#' Must not have duplicates within a `route_id` (for `BirdFlowRoutes`; but
#' not for `Routes`).
#' @param lon_vector,lat_vector Numeric vectors for longitude and latitude.
#' Must not contain missing values and must be within valid ranges.
#' @param route_type_vector A character vector of route types. Any character
#' values are accepted; non-standard values produce a warning.
#' @param x_vector,y_vector Numeric vectors for spatial coordinates. Must not
#' contain missing values.
#' @param i_vector An integer vector for spatial indices. Must not contain
#' missing values.
#' @param timestep_vector An integer vector for timesteps, paired with
#' `route_id_vector`.
#' @param species A list with species information. Must include `species_code`,
#' `scientific_name`, and `common_name`.
#' @param metadata A list with additional metadata.
#' @param dates A data frame with date-related information. Must include
#' `interval`, `date`, `midpoint`, `start`, `end`, `doy`, and `week`.
#'
#' @return Each function returns `TRUE` if validation succeeds. If validation
#' fails, an error is raised with details about the issue.
#' @seealso
#' - [Object Validators](?object_validators)
#' - [Column Targeting Functions](?target_columns)
#' @keywords internal
NULL

#' @rdname attribute_validators
validate_Routes_route_id <- function(route_id) {
  if (
    (!is.character(route_id) && !is.numeric(route_id)) ||
      any(is.na(route_id))) {
    stop(sprintf("'route_id' must be strings or numbers.
                 Missing values are not allowded."))
  }
}

#' @rdname attribute_validators
validate_Routes_date <- function(date_vector) {
  if (!inherits(date_vector, c("Date", "POSIXct", "POSIXlt")) ||
    any(is.na(date_vector))) {
    stop("'date' must be of class 'Date', 'POSIXct', or 'POSIXlt'.
         Missing values are not allowded. Please provide valid Date objects.")
  }
}

#' @rdname attribute_validators
validate_Routes_lon <- function(lon_vector) {
  if (!is.numeric(lon_vector) || any(is.na(lon_vector))) {
    stop(
      sprintf("'lon' must be a numeric vector and cannot contain NA values.")
    )
  }

  if (max(lon_vector) > 180 || min(lon_vector) > 180) {
    stop(sprintf("'lon' not in range (-180, 180)!"))
  }
}

#' @rdname attribute_validators
validate_Routes_lat <- function(lat_vector) {
  if (!is.numeric(lat_vector) || any(is.na(lat_vector))) {
    stop(
      sprintf("'lat' must be a numeric vector and cannot contain NA values.")
    )
  }

  if (max(lat_vector) > 90 || min(lat_vector) < -90) {
    stop(sprintf("'lat' not in range (-90, 90)!"))
  }
}

#' @rdname attribute_validators
validate_Routes_route_type <- function(route_type_vector) {
  valid_route_types <- c("tracking", "banding", "motus", "unknown", "synthetic")
  if (!all(unique(route_type_vector) %in% valid_route_types)) {
    invalid_types <-
      unique(route_type_vector)[!unique(route_type_vector) %in%
        valid_route_types]
    warning(sprintf(
      "Non-standard 'route_type' values: %s. The standard values are: %s.",
      paste(invalid_types, collapse = ", "),
      paste(valid_route_types, collapse = ", ")
    ))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_x <- function(x_vector) {
  if (!is.numeric(x_vector) || any(is.na(x_vector))) {
    stop(sprintf("'x' must be a numeric vector and cannot contain NA values."))
  }
}


#' @rdname attribute_validators
validate_BirdFlowRoutes_y <- function(y_vector) {
  if (!is.numeric(y_vector) || any(is.na(y_vector))) {
    stop(sprintf("'y' must be a numeric vector and cannot contain NA values."))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_i <- function(i_vector) {
  if (!is.integer(i_vector) || any(is.na(i_vector))) {
    stop(sprintf("'i' must be an integer vector and cannot contain NA values."))
  }
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_timestep <- function(timestep_vector) {
  if (!is.integer(timestep_vector) || any(is.na(timestep_vector))) {
    stop(sprintf("timestep' must be an integer vector
                 and cannot contain NA values."))
  }
  # Timestep can be duplicated (2021-01-04 and 2022-01-04 will be the same
  # timestep), but date can not be duplicated
}

#' @rdname attribute_validators
validate_BirdFlowRoutes_date <- function(route_id_vector, date_vector) {
  if (!inherits(date_vector, c("Date", "POSIXct", "POSIXlt")) ||
    any(is.na(date_vector))) {
    stop("'date' must be of class 'Date', 'POSIXct', or 'POSIXlt'.
         Missing values are not allowded. Please provide valid Date objects.")
  }

  tmp_new_df <- data.frame(
    list(route_id = route_id_vector, date_ = date_vector)
    )
  duplicated_date_check <- tmp_new_df |>
    dplyr::group_by(.data[["route_id"]]) |>
    dplyr::summarize(
      duplicated_date = any(duplicated(.data[["date_"]])),
      .groups = "drop"
    )

  if (any(duplicated_date_check$duplicated_date)) {
    stop("Duplicated date detected within one or more route IDs. Should select
         only one data point per date for each route.")
  }
  # Should not have duplicated date within each route! Should select only
  # one data point per date for each route.
  # Timestep can be duplicated (2021-01-04 and 2022-01-04 will be the same
  # timestep), but date can not be duplicated
}


#' @rdname attribute_validators
validate_BirdFlowRoutes_species <- function(species) {
  exists_names <- names(species)
  target_name_list <- c("species_code", "scientific_name", "common_name")
  for (name in target_name_list) {
    if (!(name %in% exists_names)) {
      stop(sprintf("%s component not found in species!", name))
    }
    value <- species[[name]]
    if (!(length(value) == 1 && (is.na(value) || is.character(value)))) {
      stop("species$", name, " should be NA or a single string.")
    }
  }
}


#' @rdname attribute_validators
validate_BirdFlowRoutes_metadata <- function(metadata) {
  stopifnot(inherits(metadata, "list"))
  stopifnot(all(BirdFlowRoutes_metadata_items %in% names(metadata)))
}


#' @rdname attribute_validators
validate_BirdFlowRoutes_dates <- function(dates) {
  stopifnot(inherits(dates, "data.frame"))
  existing_names <- colnames(dates)
  target_names <- c("timestep", "date", "label", "julian", "week")
  miss <- setdiff(target_names, existing_names)
  if (length(miss) > 0) {
    stop(
      "\"", paste(miss, collapse = '", "'),
      ifelse(length(miss) > 1, "\" are", "\" is"),
      " missing from dates"
    )
  }
}

#' @rdname attribute_validators
validate_BirdFlowIntervals_interval_id <- function(interval_id) {
  if ((!is.character(interval_id) && !is.numeric(interval_id)) ||
    any(is.na(interval_id))) {
    stop(sprintf("'interval_id' must be strings or numbers.
                 Missing values are not allowded."))
  }
}
