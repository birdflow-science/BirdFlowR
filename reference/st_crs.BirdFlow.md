# Get the coordinate reference system of a BirdFlow model

[`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
method for `BirdFlow` objects. Returns the coordinate reference system
stored in the model's geometry (`x$geom$crs`).

## Usage

``` r
# S3 method for class 'BirdFlow'
st_crs(x, ...)
```

## Arguments

- x:

  A `BirdFlow` object.

- ...:

  Not used.

## Value

An
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
object (class `crs`) describing the coordinate reference system of `x`.

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
sf::st_crs(bf)
#> Coordinate Reference System:
#>   User input: Custom_Lambert_Azimuthal 
#>   wkt:
#> PROJCRS["Custom_Lambert_Azimuthal",
#>     BASEGEOGCRS["WGS 84",
#>         DATUM["World Geodetic System 1984",
#>             ELLIPSOID["WGS 84",6378137,298.257223563,
#>                 LENGTHUNIT["metre",1]],
#>             ID["EPSG",6326]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["Degree",0.0174532925199433]]],
#>     CONVERSION["unnamed",
#>         METHOD["Lambert Azimuthal Equal Area",
#>             ID["EPSG",9820]],
#>         PARAMETER["Latitude of natural origin",30,
#>             ANGLEUNIT["Degree",0.0174532925199433],
#>             ID["EPSG",8801]],
#>         PARAMETER["Longitude of natural origin",-95,
#>             ANGLEUNIT["Degree",0.0174532925199433],
#>             ID["EPSG",8802]],
#>         PARAMETER["False easting",5500000,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8806]],
#>         PARAMETER["False northing",9500000,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8807]]],
#>     CS[Cartesian,2],
#>         AXIS["(E)",east,
#>             ORDER[1],
#>             LENGTHUNIT["metre",1,
#>                 ID["EPSG",9001]]],
#>         AXIS["(N)",north,
#>             ORDER[2],
#>             LENGTHUNIT["metre",1,
#>                 ID["EPSG",9001]]]]
# }
```
