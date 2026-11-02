# Convert BirdFlowRoutes to an sf object

[`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
method for `BirdFlowRoutes` objects. Converts the routes to either line
geometries (one line per route, `type = "line"`) or point geometries
(one point per route location, `type = "point"`).

## Usage

``` r
# S3 method for class 'BirdFlowRoutes'
st_as_sf(x, type = "line", crs = NULL, ...)
```

## Arguments

- x:

  A `BirdFlowRoutes` object.

- type:

  Either `"line"` (the default), to return one line per route, or
  `"point"`, to return one point per row of route data.

- crs:

  The coordinate reference system to assign to the result. If `NULL`
  (the default) the CRS is taken from `x$geom$crs` (or `x$crs`, if `x`
  has no `geom` component).

- ...:

  Not used.

## Value

An `sf` object: `LINESTRING` geometries (one row per route) if
`type = "line"`, or `POINT` geometries (one row per route location) if
`type = "point"`.

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
rts <- route(bf, n = 3)
sf::st_as_sf(rts, type = "line")
#> Set crs based on geom component.
#> Simple feature collection with 3 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 5475000 ymin: 9675000 xmax: 7575000 ymax: 11925000
#> Projected CRS: Custom_Lambert_Azimuthal
#>   route_id                       geometry
#> 1        1 LINESTRING (6375000 9975000...
#> 2        2 LINESTRING (6975000 1027500...
#> 3        3 LINESTRING (5625000 9675000...
sf::st_as_sf(rts, type = "point")
#> Set crs based on geom component.
#> Simple feature collection with 159 features and 9 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 5475000 ymin: 9675000 xmax: 7575000 ymax: 11925000
#> Projected CRS: Custom_Lambert_Azimuthal
#> First 10 features:
#>    route_id   i       lon      lat timestep       date route_type stay_id
#> 1         1 228 -85.52074 33.94934        1 2023-01-04  synthetic       1
#> 2         1 228 -85.52074 33.94934        2 2023-01-11  synthetic       1
#> 3         1 228 -85.52074 33.94934        3 2023-01-18  synthetic       1
#> 4         1 228 -85.52074 33.94934        4 2023-01-25  synthetic       1
#> 5         1 228 -85.52074 33.94934        5 2023-02-01  synthetic       1
#> 6         1 228 -85.52074 33.94934        6 2023-02-08  synthetic       1
#> 7         1 168 -84.86117 39.32858        7 2023-02-15  synthetic       2
#> 8         1 168 -84.86117 39.32858        8 2023-02-22  synthetic       2
#> 9         1 168 -84.86117 39.32858        9 2023-03-01  synthetic       2
#> 10        1 168 -84.86117 39.32858       10 2023-03-08  synthetic       2
#>    stay_len                 geometry
#> 1         5  POINT (6375000 9975000)
#> 2         5  POINT (6375000 9975000)
#> 3         5  POINT (6375000 9975000)
#> 4         5  POINT (6375000 9975000)
#> 5         5  POINT (6375000 9975000)
#> 6         5  POINT (6375000 9975000)
#> 7        11 POINT (6375000 10575000)
#> 8        11 POINT (6375000 10575000)
#> 9        11 POINT (6375000 10575000)
#> 10       11 POINT (6375000 10575000)
# }
```
