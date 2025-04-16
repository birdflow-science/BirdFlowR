## as_BirdFlowIntervals --------------------------------------------------------

#' Convert `BirdFlowRoutes` to `BirdFlowIntervals`
#'
#' @description Converts a `BirdFlowRoutes` object into a `BirdFlowIntervals`
#' object, sampling interval pairs between time points. `BirdFlowIntervals`
#' define specific movements between states in a  `BirdFlow` model.  The two
#' points in each interval will always differ in time (week  / timestep).
#' They can occupy the same location (raster cell) in the model or
#' represent a movement between two locations.
#'
#' `BirdFlowIntervals` are primarily used to evaluate model performance with
#'  `calculate_interval_metrics()`.
#'
#'
#' @param birdflow_routes A `BirdFlowRoutes` object.
#' @param max_n The maximum number of intervals to sample.
#' Defaults to 1000.
#' @param min_day_interval The minimum days required in an interval.
#' Defaults to 7.
#' @param max_day_interval The maximum days required in an interval.
#' Defaults to 180.
#' @param min_km_interval The minimum distance required for an interval.
#' Defaults to 200.
#' @param max_km_interval The maximum distance required for an interval.
#' Defaults to 2000.
#' @return A `BirdFlowIntervals` object.
#' @seealso
#' * [Routes()] for converting observational data into a formal `Routes` object
#' * [as_BirdflowRoutes()] for converting `Routes` to `BirdFlowRoutes`.
#'
#'
#' @export
#'
#' @examples
#' route_df <- data.frame(
#'   route_id = c("001", "001", "001", "001", "001", "003",
#'   "003", "003", "004"),
#'   date = as.Date(c(
#'     "2025-01-01", "2025-01-08", "2025-01-15",
#'     "2025-01-21", "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01",
#'     "2025-05-01"
#'   )),
#'   lon = c(
#'     -75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
#'     -89.6298, -85.6298, -95.3698
#'   ),
#'   lat = c(
#'     39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781,
#'     40.8781, 29.7604
#'   ),
#'   route_type = c(
#'     "tracking", "tracking", "tracking", "tracking", "tracking",
#'     "motus", "motus", "motus", "motus"
#'   )
#' )
#' routes_obj <- Routes(route_df, species = "amewoo")
#' bf <- BirdFlowModels::amewoo
#' birdflow_routes <- routes_obj |> as_BirdFlowRoutes(bf = bf)
#' birdflow_intervals <- as_BirdFlowIntervals(birdflow_routes, max_n = 1000)
as_BirdFlowIntervals <- function(birdflow_routes, max_n = 1000,
                                 min_day_interval = 7, max_day_interval = 180,
                                 min_km_interval = 200,
                                 max_km_interval = 8000) {
  stopifnot(inherits(birdflow_routes, "BirdFlowRoutes"))
  stopifnot(is.numeric(max_n))

  # Conversion
  sampling_strategy_df <- calculate_interval_sampling_strategy(
    birdflow_routes$data, max_n, min_day_interval, max_day_interval,
    min_km_interval, max_km_interval
  )

  # sampling_strategy_df: a dataframe with columns `time_points`,
  # `interval_pairs`, and `intervals_to_sample`
  intervals <- list()
  for (row_id in seq_len(nrow(sampling_strategy_df))) {
    this_row <- sampling_strategy_df[row_id, ]
    this_route <- birdflow_routes$data[
      birdflow_routes$data$route_id == this_row$route_id,
    ]

    all_pairs <- as.data.frame(t(utils::combn(seq_len(nrow(this_route)), 2)))
    all_pairs$interval_days <- abs(
      as.numeric(
        this_route[all_pairs$V2, "date"] - this_route[all_pairs$V1, "date"],
        unit = "days"
      )
    )
    all_pairs$interval_km <- great_circle_distance_lonlat_input(
      this_route[all_pairs$V1, "lat"],
      this_route[all_pairs$V1, "lon"],
      this_route[all_pairs$V2, "lat"],
      this_route[all_pairs$V2, "lon"]
    )
    all_pairs <- all_pairs[
      (all_pairs$interval_days >= min_day_interval) &
        (all_pairs$interval_days <= max_day_interval) &
        (all_pairs$interval_km >= min_km_interval) &
        (all_pairs$interval_km <= max_km_interval),
    ]
    sampled_pairs <- all_pairs[
      sample(seq_len(nrow(all_pairs)),
        size = this_row$intervals_to_sample,
        replace = FALSE
      ),
    ]
    sampled_pairs <- sampled_pairs[
      , !names(sampled_pairs) %in% c("interval_days", "interval_km")
    ]

    idx1 <- sampled_pairs[, 1]
    idx2 <- sampled_pairs[, 2]
    formatted_intervals <- data.frame(
      list(
        lon1 = this_route$lon[idx1], lon2 = this_route$lon[idx2],
        lat1 = this_route$lat[idx1], lat2 = this_route$lat[idx2],
        x1 = this_route$x[idx1], x2 = this_route$x[idx2],
        y1 = this_route$y[idx1], y2 = this_route$y[idx2],
        i1 = this_route$i[idx1], i2 = this_route$i[idx2],
        date1 = this_route$date[idx1], date2 = this_route$date[idx2],
        timestep1 = this_route$timestep[idx1],
        timestep2 = this_route$timestep[idx2],
        route_id = this_route$route_id[idx1],
        route_type = this_route$route_type[idx1]
      ),
      stringsAsFactors = FALSE # Avoid factor conversion
    )

    intervals[[row_id]] <- formatted_intervals
  }

  intervals <- do.call(rbind, intervals)

  target_columns <- get_target_columns_BirdFlowIntervals(type = "input")

  if (is.null(intervals)) {
    return(NULL)
  } else {
    intervals$i1 <- as.integer(intervals$i1)
    intervals$i2 <- as.integer(intervals$i2)
    intervals$interval_id <- paste0("interval_", seq_len(nrow(intervals)))
    rownames(intervals) <- NULL
    intervals <- intervals[
      , c(target_columns, setdiff(names(intervals), target_columns))
    ]

    obs <- BirdFlowIntervals(
      data = intervals,
      species = birdflow_routes$species,
      metadata = birdflow_routes$metadata,
      geom = birdflow_routes$geom,
      dates = birdflow_routes$dates,
      source = birdflow_routes$source
    )
    return(obs)
  }
}
