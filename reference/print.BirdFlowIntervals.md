# Print a `BirdFlowIntervals` Object

Print method for `BirdFlowIntervals` objects, summarizing interval data
and metadata, including temporal and spatial ranges.

## Usage

``` r
# S3 method for class 'BirdFlowIntervals'
print(x, ...)
```

## Arguments

- x:

  A `BirdFlowIntervals` object to print.

- ...:

  other arguments not used by this method.

## Value

Invisibly returns the input `birdflow_intervals` object.

## Examples

``` r
# Create a BirdFlowIntervals object
interval_df <- data.frame(
    interval_id = 1:3,
    route_id = c("route1", "route1", "route2"),
    lon1 = c(-90, -89, -88),
    lon2 = c(-89, -88, -87),
    lat1 = c(40, 41, 42),
    lat2 = c(41, 42, 43),
    x1 = c(1000, 1100, 1200),
    x2 = c(1100, 1200, 1300),
    y1 = c(500, 600, 700),
    y2 = c(600, 700, 800),
    i1 = as.integer(c(1, 2, 3)),
    i2 = as.integer(c(2, 3, 4)),
    date1 = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    date2 = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")),
    timestep1 = as.integer(c(1, 2, 3)),
    timestep2 = as.integer(c(2, 3, 4)),
    route_type = c("tracking", "tracking", "banding")
)
bf <- BirdFlowModels::amewoo
birdflow_intervals <- BirdFlowIntervals(interval_df,
    species = bf$species,
    metadata = NULL, geom = bf$geom, dates = get_dates(bf)
)

print(birdflow_intervals)
#> --------------------------------------------- 
#> BirdFlowIntervals Object 
#> 
#> Species:             American Woodcock / Scolopax minor / amewoo
#> Number of intervals: 3
#> Number of routes:    2
#> Date range:          2024-01-01, 2024-01-04
#> Longitude range:     -90, -87
#> Latitude range:      40, 43
#> Min. interval size:  1 days / 1 timesteps
#> Max. interval size:  1 days / 1 timesteps
#> --------------------------------------------- 
#>      Type Routes Points
#>   banding      1      1
#>  tracking      1      2
#> --------------------------------------------- 
#> Data:
#>   interval_id route_id   x1   x2  y1  y2 i1 i2 lon1 lon2 lat1 lat2      date1
#> 1           1   route1 1000 1100 500 600  1  2  -90  -89   40   41 2024-01-01
#> 2           2   route1 1100 1200 600 700  2  3  -89  -88   41   42 2024-01-02
#> 3           3   route2 1200 1300 700 800  3  4  -88  -87   42   43 2024-01-03
#>        date2 timestep1 timestep2 route_type
#> 1 2024-01-02         1         2   tracking
#> 2 2024-01-03         2         3   tracking
#> 3 2024-01-04         3         4    banding
#> --------------------------------------------- 
```
