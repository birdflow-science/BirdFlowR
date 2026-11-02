# Create `BirdFlowIntervals` objects

`as_birdflow_intervals()` creates `BirdFlowIntervals` objects from
either `BirdFlowRoutes` or a `data.frame`. An interval is a pair of
observations of a single bird at two different dates, each of which has
an associated location. The dates must be different; the location does
not need to change.

The most common use is to sample intervals from `BirdFlowRoutes` to use
for evaluating a `BirdFlow` model with the full chain being:

    rts <- as_routes(observations, species = "amewoo")
    bf_rts <- as_birdflow_routes(rts, bf)
    bf_intv <- as_birdflow_intervals(bf_rts)
    metrics <- calc_interval_metrics(bf_intv, bf)

The `data.frame` method is for advanced programmatic use. It builds a
`BirdFlowIntervals` object directly from an already-assembled interval
data frame plus the associated `species`, `metadata`, `geom`, `dates`,
and `source` components, without any sampling, allowing users full
control over how intervals are constructed.

## Usage

``` r
as_birdflow_intervals(x, ...)

# S3 method for class 'BirdFlowRoutes'
as_birdflow_intervals(
  x,
  max_n = 1000,
  min_day_interval = 7,
  max_day_interval = 180,
  min_km_interval = 200,
  max_km_interval = 8000,
  ...
)

# S3 method for class 'data.frame'
as_birdflow_intervals(x, species, metadata, geom, dates, source = NULL, ...)
```

## Arguments

- x:

  Either a `BirdFlowRoutes` object or a data frame with interval data
  (see
  [Routes-internal](https://birdflow-science.github.io/BirdFlowR/reference/Routes-internal.md)
  for the required columns).

- ...:

  Passed to methods.

- max_n:

  The maximum number of intervals to sample. Defaults to 1000. Only used
  by the `BirdFlowRoutes` method.

- min_day_interval:

  The minimum days required in an interval. Defaults to 7. Only used by
  the `BirdFlowRoutes` method.

- max_day_interval:

  The maximum days required in an interval. Defaults to 180. Only used
  by the `BirdFlowRoutes` method.

- min_km_interval:

  The minimum distance required for an interval. Defaults to 200. Only
  used by the `BirdFlowRoutes` method.

- max_km_interval:

  The maximum distance required for an interval. Defaults to 8000. Only
  used by the `BirdFlowRoutes` method.

- species:

  A list with species information. Only used by the `data.frame` method.

- metadata:

  A list with additional metadata. Only used by the `data.frame` method.

- geom:

  A list describing spatial geometry. Only used by the `data.frame`
  method.

- dates:

  A data frame with date-related information. Only used by the
  `data.frame` method.

- source:

  A character string indicating the source of the data. Only used by the
  `data.frame` method.

## Value

A `BirdFlowIntervals` object.

## See also

- [`as_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_routes.md)
  for converting observational data into a formal `Routes` object

- [`as_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_routes.md)
  for converting `Routes` to `BirdFlowRoutes`.

- [`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md)
  for calculating performance metrics from intervals and a `BirdFlow`
  object.

## Examples

``` r
observations <- data.frame(
  route_id = c("001", "001", "001", "001", "001", "003",
  "003", "003", "004"),
  date = as.Date(c(
    "2025-01-01", "2025-01-08", "2025-01-15",
    "2025-01-21", "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01",
    "2025-05-01"
  )),
  lon = c(
    -75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
    -89.6298, -85.6298, -95.3698
  ),
  lat = c(
    39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781,
    40.8781, 29.7604
  ),
  route_type = c(
    "tracking", "tracking", "tracking", "tracking", "tracking",
    "motus", "motus", "motus", "motus"
  )
)
rts <- as_routes(observations, species = "amewoo")
# \donttest{
bf <- BirdFlowExamples::amewoo
bf_rts <- rts |> as_birdflow_routes(bf = bf)
bf_intv <- as_birdflow_intervals(bf_rts, max_n = 1000)
# }
```
