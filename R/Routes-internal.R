#' @name Routes-internal
#' @title Internal (private) routes and intervals class creation functions
#' @description Internal (private) functions to create and validate
#' `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals` objects.
#'
#' These functions ensure input data meets the required structure and standard
#' for use within BirdFlow models.
#'
#' @details
#' - **`Routes()`**: Creates a `Routes` object from a data frame.
#' - **`BirdFlowRoutes()`**: Creates a `BirdFlowRoutes` object,
#' extending `Routes` with additional BirdFlow-specific spatial and
#' temporal information.
#' - **`BirdFlowIntervals()`**: Creates a `BirdFlowIntervals` object,
#' representing intervals between timesteps in BirdFlow data.
#'
#' All objects are internally validated during creation, ensuring required
#' columns, valid data types, and proper formats. Non-exported `new_*`
#' functions handle the final assembly of the object after validation.
#'
#' @param data A data frame containing route/interval data for `Routes`,
#' `BirdFlowRoutes` or `BirdFlowIntervals`.
#' @param species Either a single character that will be passed to
#' [ebirdst::get_species()] to lookup species information or a list with
#' species metadata which must include `common_name` and can optionally
#' also include `scientific_name` and  `species_code` or any other standard
#' BirdFlow species metadata. See [species_info()] for a description
#' of the full list.
#'
#' @param metadata A list with additional metadata.
#' @param geom A list describing spatial geometry,
#' such as `nrow`, `ncol`, `crs`, and `mask`.
#' @param dates A data frame with date-related information,
#' including `date`, `start`, `end`, and `timestep`.
#' @param source A character string indicating the source of the data.
#' @param sort_id_and_dates Logical. Should the data be sorted by `route_id`
#' and `dates`?
#' @param reset_index Logical. Should the index of the data frame be reset
#' after sorting?
#' @param stay_calculate_col The column name for calculating the stay_id and
#' stay_len in BirdFlowRoutes object. Default to `date`.
#' @param stay_calculate_timediff_unit The unit of stay_len in BirdFlowRoutes
#' object. Default to `days`.
#'
#' @return Each function returns an S3 object of the corresponding class
#' (`Routes`, `BirdFlowRoutes`, or `BirdFlowIntervals`).
#' @keywords internal
#' @examples
#'
#' # Examples here use private functions so can't be run except after
#' # devtools::load_all() during package development.
#' \dontrun{
#'
#' # Create a Routes object
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' species <- list(
#'   species_code = "amewoo",
#'   scientific_name = "Scolopax minor",
#'   common_name = "American Woodcock"
#' )
#' sources <- "Unknown sources"
#' routes_obj <- Routes(route_df, species = species, source = sources)
#'
#' # Create a BirdFlowRoutes object
#' ## 1. convert from `Routes`
#' bf <- BirdFlowModels::amewoo
#' birdflow_route_df <- routes_obj |> as_BirdFlowRoutes(bf = bf)
#' # the species, metadata, and sources will be inherited from the bf object.
#'
#' ## 2. Directly from dataframe
#' birdflow_route_df <- data.frame(
#'   route_id = c("001", "001", "001", "001", "001", "003", "003",
#'   "003", "004"),
#'   date = as.Date(c(
#'     "2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21", "2025-02-10",
#'     "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01"
#'   )),
#'   lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
#'   -89.6298, -85.6298, -95.3698),
#'   lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781,
#'   40.8781, 29.7604),
#'   x = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
#'   y = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
#'   i = as.integer(c(1, 2, 1, 2, 1, 2, 1, 2, 1)),
#'   timestep = as.integer(c(1, 2, 3, 4, 5, 1, 2, 3, 1)),
#'   route_type = c(
#'     "tracking", "tracking", "tracking", "tracking",
#'     "tracking", "motus", "motus", "motus", "motus"
#'   )
#' )
#' geom <- list(
#'   nrow = 100, ncol = 200, res = 1, ext = NULL, crs = NULL,
#'   mask = NULL, dynamic_mask = NULL
#' )
#' dates <- data.frame(
#'   timestep = 1:2,
#'   date = as.Date(c("2022-01-04", "2022-01-11")),
#'   label = c("January 4", "January 11"),
#'   julian = c(4, 11),
#'   week = c(1, 2)
#' )
#' birdflowroutes_object <- BirdFlowRoutes(
#'   birdflow_route_df,
#'   species = species,
#'   metadata = metadata,
#'   geom = geom,
#'   dates = dates,
#'   source = "example_source"
#' )
#' # Create a BirdFlowIntervals object
#' ## 1. convert from `BirdFlowRoutes`
#' birdflow_intervals_obj <- birdflowroutes_object |> as_BirdFlowIntervals()
#'
#' ## 2. Directly from dataframe
#' birdflow_intervals <- data.frame(
#'   interval_id = 1:3,
#'   route_id = c("route1", "route1", "route2"),
#'   lon1 = c(-90, -89, -88),
#'   lon2 = c(-89, -88, -87),
#'   lat1 = c(40, 41, 42),
#'   lat2 = c(41, 42, 43),
#'   x1 = c(1000, 1100, 1200),
#'   x2 = c(1100, 1200, 1300),
#'   y1 = c(500, 600, 700),
#'   y2 = c(600, 700, 800),
#'   i1 = as.integer(c(1, 2, 3)),
#'   i2 = as.integer(c(2, 3, 4)),
#'   date1 = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   date2 = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")),
#'   timestep1 = as.integer(c(1, 2, 3)),
#'   timestep2 = as.integer(c(2, 3, 4)),
#'   route_type = c("tracking", "tracking", "banding")
#' )
#' birdflow_intervals_obj <- BirdFlowIntervals(
#'   birdflow_intervals,
#'   species = species,
#'   metadata = metadata,
#'   geom = geom,
#'   dates = dates,
#'   source = "example_source"
#' )
#' }
#'
#' @seealso
#' - [Routes()] Create a `Routes` object
#' - [as_BirdFlowRoutes()] Convert `Routes` to `BirdFlowRoutes`
#' - [as_BirdFlowIntervals()] Extract movement between pairs of locations
#'   from [BirdFlowRoutes] for use with model evaluation.
#' - [Object Validators](?object_validators) Private functions for validating
#'   routes and intervals.
#'
NULL

#' @rdname Routes-internal
#' @keywords internal
new_Routes <- function(data, species, source) {
  # Sort columns
  target_ordered_columns <- get_target_columns_Routes(type = "output")
  data <- data[
    ,
    c(
      target_ordered_columns,
      setdiff(names(data), target_ordered_columns)
    )
  ]
  obj <- list(
    data = data,
    species = species,
    source = source
  )

  class(obj) <- c("Routes")
  return(obj)
}

#' @rdname Routes-internal
BirdFlowRoutes <- function(data,
                           species,
                           metadata,
                           geom,
                           dates,
                           source = NULL,
                           sort_id_and_dates = TRUE,
                           reset_index = FALSE,
                           stay_calculate_col = "date",
                           stay_calculate_timediff_unit = "days") {
  # Check input
  stopifnot(inherits(data, "data.frame"))
  validate_BirdFlowRoutes_birdflow_route_df(data)
  validate_BirdFlowRoutes_species(species)
  validate_BirdFlowRoutes_metadata(metadata)
  validate_geom(geom)
  validate_BirdFlowRoutes_dates(dates)

  # Make the BirdFlowRoutes object
  birdflow_routes_obj <- new_BirdFlowRoutes(
    data = data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = source,
    stay_calculate_col = stay_calculate_col,
    stay_calculate_timediff_unit = stay_calculate_timediff_unit,
    sort_id_and_dates = sort_id_and_dates
  )

  # Sort & reindex
  if (sort_id_and_dates) {
    birdflow_routes_obj$data <- birdflow_routes_obj$data |>
    sort_by_id_and_dates()
  }
  if (reset_index) {
    birdflow_routes_obj$data <- birdflow_routes_obj$data |> reset_index()
  }

  return(birdflow_routes_obj)
}

#' @rdname Routes-internal
#' @keywords internal
new_BirdFlowRoutes <- function(data, species, metadata, geom, dates, source,
                               stay_calculate_col = "date",
                               stay_calculate_timediff_unit = "days",
                               sort_id_and_dates = FALSE) {
  if (sort_id_and_dates) {
    data <- sort_by_id_and_dates(data)
  }

  ## Add stay id
  data <- data |>
    dplyr::group_by(.data$route_id) |>
    add_stay_id_with_varied_intervals(
      date_col = stay_calculate_col,
      timediff_unit = stay_calculate_timediff_unit
      ) |>
    # Here, using add_stay_id_with_varied_intervals, rather than add_stay_id.
    # It takes 'timestep' as input so account for varying intervals,
    # if the data is not sampled in a frequency.
    dplyr::ungroup() |>
    as.data.frame()

  # Sort columns
  target_ordered_columns <- get_target_columns_BirdFlowRoutes(type = "output")
  data <- data[
    ,
    c(
      target_ordered_columns,
      setdiff(
        names(data),
        target_ordered_columns
      )
    )
  ]
  obj <- list(
    data = data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = source
  )

  class(obj) <- c("BirdFlowRoutes", "Routes")

  return(obj)
}

#' @rdname Routes-internal
#' @export
BirdFlowIntervals <- function(data,
                              species,
                              metadata,
                              geom,
                              dates,
                              source = NULL) {
  validate_BirdFlowIntervals_birdflow_intervals(data)
  validate_BirdFlowRoutes_species(species)
  validate_BirdFlowRoutes_metadata(metadata)
  validate_geom(geom)
  validate_BirdFlowRoutes_dates(dates)

  # Make the BirdFlowIntervals object
  obj <- new_BirdFlowIntervals(
    data = data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = source
  )

  return(obj)
}

#' @rdname Routes-internal
#' @keywords internal
new_BirdFlowIntervals <- function(data,
                                  species,
                                  metadata,
                                  geom,
                                  dates,
                                  source) {
  # Sort columns
  target_ordered_columns <-
    get_target_columns_BirdFlowIntervals(type = "output")
  data <- data[
    ,
    c(
      target_ordered_columns,
      setdiff(
        names(data),
        target_ordered_columns
      )
    )
  ]
  obj <- list(
    data = data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = source
  )

  class(obj) <- c("BirdFlowIntervals")

  return(obj)
}



## For Routes and BirdFlowRoutes -----------------------------------------------

#' Reset Route Indices
#'
#' @description Resets the route IDs in a `Routes`
#' object to a new sequential numbering.
#'
#' @param routes A `Routes` or data frame object.
#'
#' @return A data frame with updated route IDs.
#' @keywords internal
reset_index <- function(routes) {
  stopifnot(inherits(routes, "data.frame"))
  # Get unique route_ids and create a mapping
  unique_ids <- unique(routes$route_id)
  new_ids <- paste0("route_", seq_along(unique_ids))

  # Create a lookup table
  id_mapping <- stats::setNames(new_ids, unique_ids)

  # Replace
  routes$route_id <- id_mapping[routes$route_id]

  return(routes)
}

#' Sort Routes by ID and Date
#'
#' @description Sorts a `Routes` or data frame object by route ID and date.
#'
#' @param routes A `Routes` or data frame object.
#'
#' @return A sorted data frame.
#' @export
sort_by_id_and_dates <- function(routes) {
  stopifnot(inherits(routes, "data.frame"))
  sorted_routes <- routes |>
    dplyr::arrange(.data[["route_id"]], .data[["date"]])
  return(sorted_routes)
}


#' Add Stay IDs
#'
#' @description Adds stay IDs to a data frame based on
#' changes in spatial indices.
#'
#' @param df A data frame with spatial indices.
#'
#' @return A data frame with `stay_id` and `stay_len` columns added.
#' @export
#'
#' @examples
#' routes <- data.frame(list(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = c(1, 1, 2, 2, 3, 4, 4, 5),
#'   date = as.Date(c(
#'     "2024-01-01", "2024-01-02", "2024-01-03",
#'     "2024-01-04", "2024-01-05", "2024-01-06",
#'     "2024-01-07", "2024-01-08"
#'   ))
#' ))
#' routes$i <- as.integer(routes$i)
#' df_with_stay_ids <- add_stay_id(routes)
add_stay_id <- function(df) {
  new_df <- df |>
    dplyr::mutate(
      stay_id = cumsum(c(1, as.numeric(diff(.data$i)) != 0)),
      stay_len = rep(
        rle(.data$stay_id)$lengths,
        times = rle(.data$stay_id)$lengths
      )
    )
  return(new_df)
}

#' Add Stay IDs with Temporal Thresholds
#'
#' @description Adds stay IDs to a data frame,
#' considering changes in spatial indices.
#' Should only be applied on a single route, not multiple.
#' Using add_stay_id_with_varied_intervals, rather than add_stay_id:
#' It takes 'date' as input so account for varying intervals,
#' if the data is not sampled in the same frequency.
#'
#' @param df A data frame with spatial and temporal data.
#' @param date_col The name of the column containing the
#' date information. Defaults to `"date"`.
#' @param timediff_unit The unit of 'stay_len'.
#' @return A data frame with `stay_id` and `stay_len` columns added.
#' @export
#'
#' @examples
#' routes <- data.frame(list(
#'   route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
#'   i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)), # Spatial index
#'   date = as.Date(c(
#'     "2010-01-01", "2010-01-02", "2010-01-05", "2010-01-06",
#'     "2010-01-10", "2010-01-15", "2010-01-16", "2010-01-20"
#'   )) # Time steps with varying intervals
#' ))
#' df_with_varied_stay_ids <-
#'  add_stay_id_with_varied_intervals(routes, "date", "days")
add_stay_id_with_varied_intervals <- function(
  df, date_col = "date", timediff_unit = "days"
  ) {
  # Ensure the data is sorted by timestep

  new_df <- df |>
    dplyr::mutate(
      timestep_diff = c(1, as.numeric(diff(.data[[date_col]]),
      units = timediff_unit)), # Time differences
      i_change = c(1, as.numeric(diff(.data$i)) != 0), # Changes in 'i'
      stay_id = cumsum(.data[["i_change"]])
    ) |>
    # Now the stay_id is assigned,
    # calculate the duration (time difference) of each stay
    dplyr::group_by(.data[["route_id"]], .data[["stay_id"]]) |>
    dplyr::mutate(
      stay_len = as.numeric(max(.data[[date_col]]) -
                  min(.data[[date_col]]), units = timediff_unit)
    ) |>
    dplyr::select(-dplyr::all_of(c("timestep_diff", "i_change")))

  return(new_df)
}
