# Print Routes and BirdFlowRoutes objects

Print a summary of a `Routes` and `BirdFlowRoutes` objects

## Usage

``` r
# S3 method for class 'Routes'
print(x, ...)
```

## Arguments

- x:

  A `Routes` or `BirdFlowRoutes` object to print.

- ...:

  other arguments not used by this method.

## Value

Invisibly returns the input object.

## Examples

``` r
# Create a Routes object
route_df <- data.frame(
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
routes <- Routes(route_df, species = list(common_name = "American Woodcock"))

print(routes)
#> --------------------------------------------- 
#> Routes Object 
#> 
#> Species:          American Woodcock
#> Types:            tracking, motus
#> Number of routes: 3
#> Number of points: 9
#> Date range:       2025-01-01, 2025-06-01
#> Longitude range:  -95.3698, -74.0060
#> Latitude range:   29.7604, 42.8781
#> --------------------------------------------- 
#>      Type Routes Points
#>     motus      2      4
#>  tracking      1      5
#> --------------------------------------------- 
#>   route_id       date      lon     lat route_type
#> 1      001 2025-01-01 -75.0060 39.7128   tracking
#> 2      001 2025-01-08 -75.0060 39.7128   tracking
#> 3      001 2025-01-15 -74.0060 40.7128   tracking
#> 4      001 2025-01-21 -87.6298 41.8781   tracking
#> 5      001 2025-02-10 -87.6298 41.8781   tracking
#> (4 lines omitted)

# BirdFlowRoutes
bf <- BirdFlowModels::amewoo
bf_routes <- as_BirdFlowRoutes(routes, bf)
```
