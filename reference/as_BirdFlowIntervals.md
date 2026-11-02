# Deprecated function to convert BirdFlowRoutes to BirdFlowIntervals

This function is deprecated. Please use
[`as_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_intervals.md)
instead.

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

## Value

A `BirdFlowIntervals` object.

## See also

[`as_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_intervals.md)
should be used instead of this function.
