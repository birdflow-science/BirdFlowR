# Sort Routes by ID and Date

Sorts a `Routes` or data frame object by route ID and date.

## Usage

``` r
sort_by_id_and_dates(routes)
```

## Arguments

- routes:

  A `Routes` or data frame object.

## Value

A sorted data frame.

## Examples

``` r
routes <- data.frame(list(
  route_id = c(2, 2, 1, 1),
  date = as.Date(c("2024-01-05", "2024-01-01", "2024-01-06", "2024-01-02"))
))
sort_by_id_and_dates(routes)
#>   route_id       date
#> 1        1 2024-01-02
#> 2        1 2024-01-06
#> 3        2 2024-01-01
#> 4        2 2024-01-05
```
