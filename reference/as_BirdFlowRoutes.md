# Convert Routes to BirdFlowRoutes

Convert `Routes` objects to `BirdFlowRoutes`, adding BirdFlow-specific
spatiotemporal coordinates. This may require aggregating multiple
observations within the same timestep (week) which is controlled with
the `aggregate` argument. Note the coordinates and dates in the result
will be snapped to the cell and timestep (week) centers of the BirdFlow
model (`bf`).

## Usage

``` r
as_BirdFlowRoutes(
  routes,
  bf,
  aggregate = "random",
  valid_only = TRUE,
  sort_id_and_dates = TRUE,
  reset_index = FALSE
)
```

## Arguments

- routes:

  A `Routes` object.

- bf:

  A `BirdFlow` object for spatial and temporal reference.

- aggregate:

  The aggregation method if more than one timestep is presented in a
  route. Options include `mean`, `median`, `midweek`, `random`. Default
  to `random`. See
  [snap_to_birdflow](https://birdflow-science.github.io/BirdFlowR/reference/snap_to_birdflow.md)
  for a description of each aggregation method.

- valid_only:

  Logical. Should only valid points be included? Defaults to `TRUE`.

- sort_id_and_dates:

  Logical. Should data be sorted by route ID and date? Defaults to
  `TRUE`.

- reset_index:

  Logical. Should indices be reset after sorting? Defaults to `FALSE`.

## Value

A `BirdFlowRoutes` object.

## See also

- [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
  for creating synthetic routes from a `BirdFlow` model.

- [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md)
  for converting observational data into a formal `Routes` object
  suitable for use with this function.

- [`plot_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md)
  for plotting arguments used when calling
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on `Routes`
  and `BirdFlowRoutes` objects.

- [`snap_to_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/snap_to_birdflow.md)
  to align observational data with a BirdFlow model without making a
  formal `Routes ` object. This function also provides more details when
  errors arise - usually due to the data not overlapping the modeled
  states as defined by the mask and dynamic mask within `bf`.

- [`as_BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowIntervals.md)
  for making intervals from the `BirdFlowModels` `BirdFlowIntervals`
  define movements between pair of locations. Typically they are used to
  evaluate model performance.

## Examples

``` r
route_data <- data.frame(
  route_id = c("001", "001", "001", "001", "001",
  "003", "003", "003", "004"),
  date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21",
  "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
  lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
  -89.6298, -85.6298, -95.3698),
  lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781,
  42.8781, 40.8781, 29.7604),
  route_type = c("tracking", "tracking", "tracking", "tracking",
  "tracking", "motus", "motus", "motus", "motus")
)
bf <- BirdFlowModels::amewoo
routes <- Routes(route_data, species = species(bf), source = "Pkg. example")

bf_routes <- as_BirdFlowRoutes(routes, bf)
```
