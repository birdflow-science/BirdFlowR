# Add Stay IDs

Adds stay IDs to a data frame based on changes in spatial indices.

## Usage

``` r
add_stay_id(df)
```

## Arguments

- df:

  A data frame with spatial indices.

## Value

A data frame with `stay_id` and `stay_len` columns added.

## Examples

``` r
routes <- data.frame(list(
  route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
  i = c(1, 1, 2, 2, 3, 4, 4, 5),
  date = as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03",
    "2024-01-04", "2024-01-05", "2024-01-06",
    "2024-01-07", "2024-01-08"
  ))
))
routes$i <- as.integer(routes$i)
df_with_stay_ids <- add_stay_id(routes)
```
