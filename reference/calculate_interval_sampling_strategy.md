# Calculate Interval Sampling Strategy

Internal function used by
[`as_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md)
to determine how many intervals to sample from each route based on the
total number of intervals requested. Ensures an even distribution across
routes when possible.

## Usage

``` r
calculate_interval_sampling_strategy(
  routes,
  n,
  min_day_interval,
  max_day_interval,
  min_km_interval,
  max_km_interval
)
```

## Arguments

- routes:

  A data frame similar to the data feature in `Routes` – with columns
  `route_id`, `date`, `lon` and `lat`.

- n:

  The total maximum number of intervals to sample. Notice: The actual
  output of intervals might be less than n, because of data deficiency.
  But never larger than n.

- min_day_interval:

  The minimum days required in an interval.

- max_day_interval:

  The maximum days required in an interval.

- min_km_interval:

  The minimum distance required for an interval.

- max_km_interval:

  The maximum distance required for an interval.

## Value

A data frame with the columns:

- `route_id`: The route ID.

- `time_points`: The number of time points in the route.

- `interval_pairs`: The total number of possible interval pairs for the
  route.

- `intervals_to_sample`: The number of intervals to sample for the
  route.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
routes <- data.frame(
  route_id = c("A", "A", "B", "B", "B"),
  lon = c(100, 101, 102, 103, 104),
  lat = c(40, 42, 44, 46, 48),
  date = as.Date("2024-01-01") + (0:4) * 10
)
sampling_strategy <- calculate_interval_sampling_strategy(routes,
  n = 10,
  min_day_interval = 20, min_km_interval = 100
)
} # }
```
