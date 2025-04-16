# Required metadata items for BirdFlowRoutes used in route() and
# as_BirdFlowRoutes
BirdFlowRoutes_metadata_items <- c("n_active", "ebird_version_year")


#' Convert Routes to BirdFlowRoutes
#'
#' @description Convert `Routes` objects to `BirdFlowRoutes`,
#' adding BirdFlow-specific spatiotemporal coordinates.
#' This may require aggregating multiple observations within the same timestep
#' (week) which is controlled with the `aggregate` argument.
#' Note the coordinates and dates in the result will be snapped to the
#' cell and timestep (week) centers of the BirdFLow model (`bf`).
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
#' * [plot_routes()] for plotting arguments used when calling `plot()` on
#' `Routes` and `BirdFlowRoutes` objects.
#' * [snap_to_birdflow()] to align observational data with a BirdFlow model
#' without making a formal  `Routes ` object. This function also provides more
#' details when errors arise - usually due to the data not overlapping the
#' modeled states as defined by the mask and dynamic mask within `bf`.
#' * [as_BirdFlowIntervals()] for making intervals from the `BirdFlowModels`
#'  `BirdFlowIntervals` define movements between pair of locations. Typically
#'  they are used to evaluate model performance.
#' @return A `BirdFlowRoutes` object.
#' @export
#'
#' @examples
#' route_data <- data.frame(
#'   route_id = c("001", "001", "001", "001", "001",
#'   "003", "003", "003", "004"),
#'   date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21",
#'   "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
#'   lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
#'   -89.6298, -85.6298, -95.3698),
#'   lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781,
#'   42.8781, 40.8781, 29.7604),
#'   route_type = c("tracking", "tracking", "tracking", "tracking",
#'   "tracking", "motus", "motus", "motus", "motus")
#' )
#' bf <- BirdFlowModels::amewoo
#' routes <- Routes(route_data, species = species(bf), source = "Pkg. example")
#'
#' bf_routes <- as_BirdFlowRoutes(routes, bf)
#'
as_BirdFlowRoutes <- function(routes, bf, aggregate = "random",
                              valid_only = TRUE, sort_id_and_dates = TRUE,
                              reset_index = FALSE) {
  # Check input
  stopifnot(inherits(routes, "Routes"))
  stopifnot(inherits(bf, "BirdFlow"))
  stopifnot(is.logical(sort_id_and_dates))
  stopifnot(is.logical(reset_index))
  stopifnot(is.logical(valid_only))

  # Sort & reindex
  if (sort_id_and_dates) {
    routes$data <- (routes$data |> sort_by_id_and_dates())
  }
  if (reset_index) {
    routes$data <- (routes$data |> reset_index())
  }

  # Conversion
  original_routes_info <- routes$data |>
    dplyr::select(dplyr::any_of(c("route_id", "route_type", "info")))

  routes$data <- snap_to_birdflow(
    routes$data,
    bf = bf,
    x_col = "lon", y_col = "lat",
    date_col = "date",
    id_cols = "route_id",
    crs = "EPSG:4326",
    aggregate = aggregate
  )

  # Only successfully converted spatiotemporal points will be included
  if (valid_only) {
    routes$data <- routes$data |>
      dplyr::filter(!is.na(.data[["x"]]) &
        !is.na(.data[["y"]]) &
        !is.na(.data[["i"]]) &
        !is.na(.data[["timestep"]]) &
        !.data[["error"]])

    if (nrow(routes$data) == 0) {
      stop("All points falling fail to convert to the BirdFlow spatiotemporal
           coordinates.")
    }
  }

  # Note snap_to_birdflow resolves the timestep and  location index (i)
  # that coordinate with a cell center but doesn't update the date
  # or x,y coordinates.  Here we move the coordinates to the week and cell
  # center.
  years <- lubridate::year(routes$data$date)
  routes$data$date <- lookup_date(routes$data$timestep, bf)
  lubridate::year(routes$data$date) <- years
  routes$data[, c("x", "y")] <- i_to_xy(routes$data$i, bf)


  # add some attributes (e.g., lon and lat) back, and convert data type.
  # lat,lon are now cell centers
  latlon <- xy_to_latlon(x = routes$data$x, y = routes$data$y, bf = bf)
  routes$data$lat <- latlon$lat
  routes$data$lon <- latlon$lon
  routes$data <- merge(routes$data, original_routes_info |>
    dplyr::distinct(),
  by = "route_id", all.x = TRUE
  )
  routes$data$timestep <- as.integer(routes$data$timestep)
  routes$data$i <- as.integer(routes$data$i)
  routes$data <- routes$data |>
    dplyr::select(-dplyr::all_of(c("n", "error", "message")))

  # Transform species to the BirdFlow species list
  if (!routes$species$common_name == bf$species$common_name) {
    warning("The BirdFlow model species, ",
            '"', bf$species$common_name, "',",
            " Does not match Routes, ",
            '"', routes$species$common_name, '"')
  }
  # The model species information - which should always be complete -
  # takes precedence.
  species <- bf$species
  geom <- bf$geom
  dates <- get_dates(bf) # use the up-to-date dates dataframe

  # Make metadata
  metadata <- bf$metadata[BirdFlowRoutes_metadata_items]

  # Transform to BirdFlowRoutes
  routes <- BirdFlowRoutes(
    data = routes$data,
    species = species,
    metadata = metadata,
    geom = geom,
    dates = dates,
    source = routes$source,
    sort_id_and_dates = sort_id_and_dates,
    reset_index = reset_index
  )
  return(routes)
}
