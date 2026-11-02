# Deprecated function to convert Routes to BirdFlowRoutes

This function is deprecated. Please use
[`as_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_routes.md)
instead.

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

[`as_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_routes.md)
should be used instead of this function.
