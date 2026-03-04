# Add Stay IDs with Temporal Thresholds

Adds stay IDs to a data frame, considering changes in spatial indices.
Should only be applied on a single route, not multiple. Using
`add_stay_id_with_varied_intervals()`, rather than
[`add_stay_id()`](https://birdflow-science.github.io/BirdFlowR/reference/add_stay_id.md):
It takes `date` as input so account for varying intervals, if the data
is not sampled in the same frequency.

## Usage

``` r
add_stay_id_with_varied_intervals(
  df,
  date_col = "date",
  timediff_unit = "days"
)
```

## Arguments

- df:

  A data frame with spatial and temporal data.

- date_col:

  The name of the column containing the date information. Defaults to
  `"date"`.

- timediff_unit:

  The unit of `stay_len`.

## Value

A data frame with `stay_id` and `stay_len` columns added.

## Examples

``` r
routes <- data.frame(list(
  route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
  i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)), # Spatial index
  date = as.Date(c(
    "2010-01-01", "2010-01-02", "2010-01-05", "2010-01-06",
    "2010-01-10", "2010-01-15", "2010-01-16", "2010-01-20"
  )) # Time steps with varying intervals
))
df_with_varied_stay_ids <-
 add_stay_id_with_varied_intervals(routes, "date", "days")
```
