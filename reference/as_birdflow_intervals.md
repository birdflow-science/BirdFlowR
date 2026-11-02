# Make or convert to a `BirdFlowIntervals` object

`as_birdflow_intervals()` is a generic function with two methods:

- The `BirdFlowRoutes` method converts a `BirdFlowRoutes` object into a
  `BirdFlowIntervals` object, sampling interval pairs between time
  points. `BirdFlowIntervals` define specific movements between states
  in a `BirdFlow` model. The two points in each interval will always
  differ in time (week / timestep). They can occupy the same location
  (raster cell) in the model or represent a movement between two
  locations.

- The `data.frame` method builds a `BirdFlowIntervals` object directly
  from an already-assembled interval data frame plus the associated
  `species`, `metadata`, `geom`, `dates`, and `source` components,
  without any sampling.

`BirdFlowIntervals` are primarily used to evaluate model performance
with
[`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md).

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

  The maximum distance required for an interval. Defaults to 2000. Only
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

## Examples

``` r
route_df <- data.frame(
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
routes_obj <- as_routes(route_df, species = "amewoo")
# \donttest{
bf <- BirdFlowExamples::amewoo
birdflow_routes <- routes_obj |> as_birdflow_routes(bf = bf)
birdflow_intervals <- as_birdflow_intervals(birdflow_routes, max_n = 1000)
# }
```
