#' Calculate Interval Sampling Strategy
#'
#' @description Internal function used by `as_BirdFlowRoutes()` to determine
#' how many intervals to sample from each route based on
#' the total number of intervals requested.
#' Ensures an even distribution across routes when possible.
#'
#' @param routes A data frame similar to the data feature in `Routes` --
#' with columns `route_id`, `date`, `lon` and `lat`.
#' @param n The total maximum number of intervals to sample. Notice:
#' The actual output of intervals might be less than n,
#' because of data deficiency. But never larger than n.
#' @param min_day_interval The minimum days required in an interval.
#' @param max_day_interval The maximum days required in an interval.
#' @param min_km_interval The minimum distance required for an interval.
#' @param max_km_interval The maximum distance required for an interval.
#' @return A data frame with the columns:
#' - `route_id`: The route ID.
#' - `time_points`: The number of time points in the route.
#' - `interval_pairs`: The total number of possible interval
#' pairs for the route.
#' - `intervals_to_sample`: The number of intervals to sample for the route.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Example usage
#' routes <- data.frame(
#'   route_id = c("A", "A", "B", "B", "B"),
#'   lon = c(100, 101, 102, 103, 104),
#'   lat = c(40, 42, 44, 46, 48),
#'   date = as.Date("2024-01-01") + (0:4) * 10
#' )
#' sampling_strategy <- calculate_interval_sampling_strategy(routes,
#'   n = 10,
#'   min_day_interval = 20, min_km_interval = 100
#' )
#' }
calculate_interval_sampling_strategy <- function(
    routes, n, min_day_interval,
    max_day_interval, min_km_interval, max_km_interval) {
  # Group by route_id and count the number of time points in each route
  route_counts <- routes |>
    dplyr::group_by(.data[["route_id"]]) |>
    dplyr::summarize(time_points = dplyr::n())

  # Calculate interval pairs valid for sampling for each route
  route_counts <- routes |>
    dplyr::group_by(.data[["route_id"]]) |>
    dplyr::summarize(
      interval_pairs = {
        if (length(.data[["date"]]) > 1) {
          date_ <- as.Date(.data[["date"]])
          diff_matrix <- abs(outer(date_, date_, "-")) # differences in dates

          sf_points <- sf::st_as_sf(
            data.frame(
              lon = .data[["lon"]],
              lat = .data[["lat"]]
            ),
            coords = c("lon", "lat"), crs = 4326
          )
          distance_matrix <- sf::st_distance(sf_points, sf_points) |>
            units::set_units("km") |>
            units::drop_units()

          valid_pairs <- sum(
            diff_matrix[upper.tri(diff_matrix)] >= min_day_interval &
              diff_matrix[upper.tri(diff_matrix)] <= max_day_interval &
              distance_matrix[upper.tri(distance_matrix)] >= min_km_interval &
              distance_matrix[upper.tri(distance_matrix)] <= max_km_interval
          ) # Total valid pairs
        } else {
          valid_pairs <- 0
        }
        valid_pairs
      }
    ) |>
    # Filter routes with at least one valid pair
    dplyr::filter(.data[["interval_pairs"]] > 0)

  if (n <= nrow(route_counts)) {
    # The requested interval count is smaller than the total routes,
    # so sample one interval per route
    sampled_route_counts <- route_counts |>
      dplyr::slice_sample(n = n, replace = FALSE)
    sampled_route_counts$intervals_to_sample <- 1
  } else if (n >= sum(route_counts$interval_pairs)) {
    # Don't need to think -- We can't generate that much intervals!
    # So sample all of them
    sampled_route_counts <- route_counts
    sampled_route_counts$intervals_to_sample <-
      sampled_route_counts$interval_pairs
  } else {
    # Calculate how many intervals to sample for each route
    ## Fist, we should have one sample per route, to give an even sampling
    part1 <- nrow(route_counts)
    rest <- n - part1

    rest_samplable_route_counts <- route_counts
    rest_samplable_route_counts$sampable_amount <-
      rest_samplable_route_counts$interval_pairs - 1
    rest_samplable_route_counts$proportion <-
      rest_samplable_route_counts$sampable_amount /
        sum(rest_samplable_route_counts$sampable_amount)
    rest_samplable_route_counts$to_sample <-
      pmin(
        ceiling(rest_samplable_route_counts$proportion * rest) + 1,
        rest_samplable_route_counts$sampable_amount
      )

    if (sum(rest_samplable_route_counts$to_sample) > rest) {
      # Because of the ceiling rounding,
      # the total sampling planned might exceed the `rest`,
      # In that case, reduce the amount based on proportion, step by step
      to_reduce <- sum(rest_samplable_route_counts$to_sample) - n

      while (to_reduce > 0) {
        # Apply proportional reduction, constrained by `to_reduce`
        sampled_index <- sample(
          seq_len(nrow(rest_samplable_route_counts)),
          size = 1,
          prob = rest_samplable_route_counts$proportion
        )

        if (rest_samplable_route_counts[sampled_index, "to_sample"] <= 1) {
          # Should leave at least one sample for that route
          print(sampled_index)
          print(rest_samplable_route_counts[sampled_index, "to_sample"])
          next
        }
        rest_samplable_route_counts[sampled_index, "to_sample"] <-
          rest_samplable_route_counts[sampled_index, "to_sample"] - 1
        to_reduce <- to_reduce - 1
      }

      sampled_route_counts <- route_counts
      sampled_route_counts$intervals_to_sample <-
        rest_samplable_route_counts$to_sample
    } else if (sum(rest_samplable_route_counts$to_sample) < rest) {
      stop("Not possible error. The sampling has problems.")
    }
  }
  return(sampled_route_counts)
}