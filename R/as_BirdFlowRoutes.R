# Required metadata items for BirdFlowRoutes used in route() and
# as_BirdFlowRoutes
BirdFlowRoutes_metadata_items <- c("n_active", "ebird_version_year")


#' Convert Routes to BirdFlowRoutes
#'
#' @description Convert `Routes` objects to `BirdFlowRoutes`,
#' adding BirdFlow-specific spatiotemporal coordinates.
#' This may require aggregating multiple observations within the same timestep
#' (week) which is controlled with the `aggregate` argument.
#' @param routes A `Routes` object.
#' @param bf A `BirdFlow` object for spatial and temporal reference.
#' @param aggregate The aggregation method if more than one timestep is
#' presented in a route.
#' Options include `mean`, `median`, `midweek`, `random`. Default to `random`.
#' See [snap_to_birdflow] for a description of each aggregation method.
#' @param valid_only Logical. Should only valid points be included?
#' Defaults to `TRUE`.
#' @param sort_id_and_dates Logical. Should data be sorted by route ID and date?
#'  Defaults to `TRUE`.
#' @param reset_index Logical. Should indices be reset after sorting?
#' Defaults to `FALSE`.
#' @seealso
#' * [route()] for creating synthetic routes from a `BirdFlow` model.
#' * [Routes()] for converting observational data into a formal `Routes` object
#' suitable for use with this function.
#' * [plot_routes()] for plotting arguments used when calling `plot` on
#' `Routes` and `BirdFlowRoutes` objects.
#' `plot`
#' * [snap_to_birdflow()] to align observational data with a BirdFlow model
#' without making a formal  `Routes ` object
#' * [as_BirdFlowIntervals()] for making intervals from the `BirdFlowModels`
#'  `BirdFlowIntervalse` define movements between pair of locations,
#'  which can used to evaluate model performance.
#' @return A`BirdFlowRoutes` object.
#' @export
#'
#' @examples
#' route_data <- data.frame(
#'   route_id = 1:3,
#'   date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   lon = c(-90, -89, -88),
#'   lat = c(40, 41, 42),
#'   route_type = c("tracking", "banding", "unknown")
#' )
#' bf <- BirdFlowModels::amewoo
#' routes<- Routes(route_data,   species = species(bf), source = "Pkg. example")
#'
#' bf_routes <- as_BirdFlowRoutes(routes, bf)
#'
as_BirdFlowRoutes <- function(routes, bf, aggregate = 'random',
                              valid_only = TRUE, sort_id_and_dates = TRUE,
                              reset_index=FALSE){
  # Check input
  stopifnot(inherits(routes, 'Routes'))
  stopifnot(inherits(bf, 'BirdFlow'))
  stopifnot(is.logical(sort_id_and_dates))
  stopifnot(is.logical(reset_index))
  stopifnot(is.logical(valid_only))

  # Sort & reindex
  if (sort_id_and_dates){
    routes$data <- (routes$data |> sort_by_id_and_dates())
  }
  if (reset_index){
    routes$data <- (routes$data |> reset_index())
  }

  # Conversion
  original_routes_info <- routes$data |> dplyr::select(dplyr::any_of(c('route_id', 'route_type','info')))

  routes$data <- snap_to_birdflow(
    routes$data,
    bf=bf,
    x_col = "lon", y_col = "lat",
    date_col = "date",
    id_cols = "route_id",
    crs = "EPSG:4326",
    aggregate = aggregate)

  # Only successfully converted spatiotemporal points will be included
  if (valid_only){
    routes$data <- routes$data |>
      dplyr::filter(!is.na(.data[['x']]) & !is.na(.data[['y']]) & !is.na(.data[['i']]) & !is.na(.data[['timestep']]) & !.data[['error']])
  }

  # add some attributes (e.g., lon and lat) back, and convert data type.
  latlon <- xy_to_latlon(x=routes$data$x, y=routes$data$y, bf=bf)
  routes$data$lat <- latlon$lat
  routes$data$lon <- latlon$lon
  routes$data <- merge(routes$data, original_routes_info |> dplyr::distinct(), by='route_id', all.x=TRUE)
  routes$data$timestep <- as.integer(routes$data$timestep)
  routes$data$i <- as.integer(routes$data$i)
  routes$data <- routes$data |> dplyr::select(-dplyr::all_of(c('n', 'error', 'message')))

  # Transform species to the BirdFlow species list
  species <- bf$species # Regardless of what the species in the `routes` is -- if using bf, then the species is the species of bf model.
  geom <- bf$geom
  dates <- get_dates(bf) # use the up-to-date dates dataframe

  # Make metadata
  metadata <- bf$metadata[BirdFlowRoutes_metadata_items]

  # Transform to BirdFlowRoutes
  routes <- BirdFlowRoutes(data = routes$data,
                           species = species,
                           metadata = metadata,
                           geom = geom,
                           dates = dates,
                           source = routes$source)
  return(routes)
}




