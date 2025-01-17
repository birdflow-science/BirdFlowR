#' @name RouteDataClass
#' @title RouteDataClass Creation Functions
#' @description Functions to create and validate `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals` objects.
#' These functions ensure input data meets the required structure and standards for use within BirdFlow models.
#'
#' @details 
#' - **`Routes()`**: Creates a `Routes` object from a data frame.
#' - **`BirdFlowRoutes()`**: Creates a `BirdFlowRoutes` object, extending `Routes` with additional BirdFlow-specific spatial and temporal information.
#' - **`BirdFlowIntervals()`**: Creates a `BirdFlowIntervals` object, representing intervals between timesteps in BirdFlow data.
#'
#' All objects are internally validated during creation, ensuring required columns, valid data types, and proper formats. Non-exported `new_*` functions handle the final assembly of the object after validation.
#'
#' @param route_df A data frame containing route data for `Routes`.
#' @param birdflow_route_df A data frame containing route data for `BirdFlowRoutes`.
#' @param birdflow_intervals A data frame containing interval data for `BirdFlowIntervals`.
#' @param species A list with species metadata, including `species_code`, `scientific_name`, and `common_name`.
#' @param geom A list describing spatial geometry, such as `nrow`, `ncol`, `crs`, and `mask`.
#' @param dates A data frame with date-related information, including `date`, `start`, `end`, and `timestep`.
#' @param source A character string indicating the source of the data.
#' @param sort_id_and_dates Logical. Should the data be sorted by `route_id` and `dates`?
#' @param reset_index Logical. Should the index of the data frame be reset after sorting?
#'
#' @return Each function returns an S3 object of the corresponding class (`Routes`, `BirdFlowRoutes`, or `BirdFlowIntervals`).
#'
#' @examples
#' # Create a Routes object
#' route_df <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' Routes(route_df)
#'
#' # Create a BirdFlowRoutes object
#' birdflow_route_df <- data.frame(
#'   route_id = c("route1", "route2"),
#'   x = c(1000, 2000),
#'   y = c(500, 600),
#'   i = as.integer(c(1, 2)),
#'   timestep = as.integer(c(1, 2)),
#'   date = as.Date(c("2024-01-01", "2024-01-02")),
#'   route_type = c("tracking", "banding")
#' )
#' BirdFlowRoutes(
#'   birdflow_route_df,
#'   species = list(
#'     species_code = "SP001",
#'     scientific_name = "Hirundo rustica",
#'     common_name = "Barn Swallow"
#'   ),
#'   geom = list(nrow = 100, ncol = 200, crs = NULL, mask = NULL),
#'   dates = data.frame(
#'     interval = 1:2,
#'     date = as.Date(c("2024-01-01", "2024-01-02")),
#'     midpoint = as.Date(c("2024-01-01", "2024-01-02"))
#'   )
#' )
#'
#' @seealso
#' - [Object Validators](?object_validators)
#' - [as_BirdFlowRoutes](?as_BirdFlowRoutes)
#' - [as_BirdFlowIntervals](?as_BirdFlowIntervals)
#' 
NULL

#' @rdname RouteDataClass
#' @export
Routes <- function(route_df, species = NULL, source = NULL){
  # Check input
  stopifnot(is.data.frame(route_df))
  validate_Routes_route_df(route_df)
  
  # Make new Routes object
  obj <- new_Routes(route_df, species, source)
  return(obj)
}

#' @rdname RouteDataClass
#' @keywords internal
new_Routes <- function(route_df, species, source){
  # Sort columns
  target_ordered_columns <- get_target_columns_Routes(type='output')
  route_df <- route_df[, c(target_ordered_columns, setdiff(names(route_df), target_ordered_columns))]
  
  obj <- structure(
    route_df, 
    class = c("Routes", class(route_df)),
    species = species, 
    source = source
  )
  return(obj)
}

#' @rdname RouteDataClass
#' @export
BirdFlowRoutes <- function(birdflow_route_df,
                           species,
                           geom,
                           dates,
                           source = NULL,
                           sort_id_and_dates = TRUE,
                           reset_index=FALSE){
  # Check input
  stopifnot(inherits(bf, 'BirdFlow'))
  stopifnot(inherits(birdflow_route_df, 'data.frame'))
  validate_BirdFlowRoutes_birdflow_route_df(birdflow_route_df)
  validate_BirdFlowRoutes_species(species)
  validate_BirdFlowRoutes_geom(geom)
  validate_BirdFlowRoutes_dates(dates)
  
  # Sort & reindex
  if (sort_id_and_dates){
    birdflow_route_df <- birdflow_route_df |> sort_by_id_and_dates()
  }
  if (reset_index){
    birdflow_route_df <- birdflow_route_df |> reset_index()
  }
  
  # Make the BirdFlowRoutes object
  obj <- new_BirdFlowRoutes(birdflow_route_df=birdflow_route_df, 
                            species=species,
                            geom=geom,
                            dates=dates,
                            source=source)
  
  return(obj)
}

#' @rdname RouteDataClass
#' @keywords internal
new_BirdFlowRoutes <- function(birdflow_route_df, species, geom, dates, source){
  
  ## Add stay id
  birdflow_route_df <- birdflow_route_df |>
    dplyr::group_by(.data$route_id) |>
    add_stay_id_with_varied_intervals(timestep_col = "timestep") |> # Here, using add_stay_id_with_varied_intervals, rather than add_stay_id. It takes 'timestep' as input so account for varying intervals, if the data is not sampled in a frequency.
    dplyr::ungroup() |>
    as.data.frame() |>
    preserve_s3_attributes(original=birdflow_route_df)
  
  # Sort columns
  target_ordered_columns <- get_target_columns_BirdFlowRoutes(type='output')
  birdflow_route_df <- birdflow_route_df[, c(target_ordered_columns, setdiff(names(birdflow_route_df), target_ordered_columns))]
  
  obj <- structure(
    birdflow_route_df, 
    class = unique(c('BirdFlowRoutes', 'Routes', class(birdflow_route_df))),
    species = species, 
    geom = geom,
    dates = dates,
    source = source
  )
  return(obj)
}

#' @rdname RouteDataClass
#' @export
BirdFlowIntervals <- function(birdflow_intervals,
                              species,
                              geom,
                              dates,
                              source = NULL) {
  
  validate_BirdFlowIntervals_birdflow_intervals(birdflow_intervals)
  validate_BirdFlowRoutes_species(species)
  validate_BirdFlowRoutes_geom(geom)
  validate_BirdFlowRoutes_dates(dates)
  
  # Make the BirdFlowIntervals object
  obj <- new_BirdFlowIntervals(birdflow_intervals=birdflow_intervals, 
                            species=species,
                            geom=geom,
                            dates=dates,
                            source=source)
  
  return(obj)
}

#' @rdname RouteDataClass
#' @keywords internal
new_BirdFlowIntervals <- function(birdflow_intervals,
                                  species,
                                  geom,
                                  dates,
                                  source){
  
  # Sort columns
  target_ordered_columns <- get_target_columns_BirdFlowIntervals(type='output')
  birdflow_intervals <- birdflow_intervals[, c(target_ordered_columns, setdiff(names(birdflow_intervals), target_ordered_columns))]
  
  obj <- structure(
    birdflow_intervals, 
    class = unique(c('BirdFlowIntervals', class(birdflow_intervals))),
    species = species,
    geom = geom,
    dates = dates,
    source = source
  )
  return(obj)
}

