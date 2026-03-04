# Convert `BirdFlowRoutes` to `BirdFlowIntervals`

Converts a `BirdFlowRoutes` object into a `BirdFlowIntervals` object,
sampling interval pairs between time points. `BirdFlowIntervals` define
specific movements between states in a `BirdFlow` model. The two points
in each interval will always differ in time (week / timestep). They can
occupy the same location (raster cell) in the model or represent a
movement between two locations.

`BirdFlowIntervals` are primarily used to evaluate model performance
with `calculate_interval_metrics()`.

## Usage

``` r
as_BirdFlowIntervals(
  birdflow_routes,
  max_n = 1000,
  min_day_interval = 7,
  max_day_interval = 180,
  min_km_interval = 200,
  max_km_interval = 8000
)
```

## Arguments

- birdflow_routes:

  A `BirdFlowRoutes` object.

- max_n:

  The maximum number of intervals to sample. Defaults to 1000.

- min_day_interval:

  The minimum days required in an interval. Defaults to 7.

- max_day_interval:

  The maximum days required in an interval. Defaults to 180.

- min_km_interval:

  The minimum distance required for an interval. Defaults to 200.

- max_km_interval:

  The maximum distance required for an interval. Defaults to 2000.

## Value

A `BirdFlowIntervals` object.

## See also

- [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md)
  for converting observational data into a formal `Routes` object

- [`as_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md)
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
routes_obj <- Routes(route_df, species = "amewoo")
bf <- BirdFlowModels::amewoo
birdflow_routes <- routes_obj |> as_BirdFlowRoutes(bf = bf)
birdflow_intervals <- as_BirdFlowIntervals(birdflow_routes, max_n = 1000)
```
