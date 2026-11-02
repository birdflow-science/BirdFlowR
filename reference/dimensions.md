# Dimensions of a BirdFlow object

Functions to return BirdFlow model dimensions and other basic
information

## Usage

``` r
# S3 method for class 'BirdFlow'
nrow(x)

# S3 method for class 'BirdFlow'
ncol(x)

# S3 method for class 'BirdFlow'
dim(x)

n_timesteps(x)

n_distr(x)

n_transitions(x)

n_active(x)

# S4 method for class 'BirdFlow'
crs(x, proj = FALSE, describe = FALSE, parse = FALSE)

# S4 method for class 'BirdFlow'
ext(x)

# S4 method for class 'BirdFlow'
res(x)

# S4 method for class 'BirdFlow'
xres(x)

# S4 method for class 'BirdFlow'
yres(x)

# S4 method for class 'BirdFlow'
xmin(x)

# S4 method for class 'BirdFlow'
ymin(x)

# S4 method for class 'BirdFlow'
xmax(x)

# S4 method for class 'BirdFlow'
ymax(x)

is_cyclical(x)

n_parameters(x)
```

## Arguments

- x:

  A BirdFlow object

- proj:

  logical. If `TRUE` the crs is returned in PROJ-string notation

- describe:

  logical. If `TRUE` the name, EPSG code, and the name and extent of the
  area of use are returned if known

- parse:

  logical. If `TRUE`, wkt parts are parsed into a vector (each line
  becomes an element)

## Value

`nrow()` returns the number of rows in the raster extent of the BirdFlow
model

`ncol()` number of columns in the raster extent

`dim()` number of rows and columns in the raster extent.

`n_timesteps()` number of timesteps distributions.

`n_distr()` number of distributions

`n_transitions()` number of transitions, if the model is circular in
time this will equal `n_timesteps()`.

`n_active()` number of active cells (locations).

`crs()` the coordinate reference system, a character with well known
text (WKT) by default, but see arguments.

`ext()` a
[SpatExtent](https://rspatial.github.io/terra/reference/SpatExtent-class.html)
object, which contains the xmin, xmax, ymin, and ymax of the extent.

`res()` two numbers, the cell width and height (x and y resolution).

`xres()` the width of the cells (x resolution).

`yres()` the height of the cells (y resolution).

`xmin()` minimum x coordinate of extent.

`ymin()` minimum y coordinate of extent.

`xmax()` maximum x coordinate of extent.

`ymax()` maximum y coordinate of extent.

`is_cyclical()` returns `TRUE` if the BirdFlow model has a transition
from the last timestep to the first and `FALSE` otherwise.

`n_parameters()` the number of of parameters that the BirdFlow model
contains or will contain. This is the number of cells in the marginal
matrices + the sum of the dynamic mask for the first timestep (number of
unmasked cells at the first timestep). If the model isn't dynamically
masked this is equivalent to
`n_active(x)^2 * n_transitons(x) + n_active(x)`

## See also

terra defines the S4 generics for
[crs()](https://rspatial.github.io/terra/reference/crs.html),
[ext()](https://rspatial.github.io/terra/reference/ext.html), res(),
xres(), and yres()

[`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md)
returns distributions from a `BirdFlow` object.

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
nrow(bf)
#> [1] 20
ncol(bf)
#> [1] 22
dim(bf)
#> [1] 20 22
n_timesteps(bf)
#> [1] 52
n_distr(bf)
#> [1] 52
n_transitions(bf)
#> [1] 52
n_active(bf)
#> [1] 283
is_cyclical(bf)
#> [1] TRUE
crs(bf)
#> [1] "PROJCRS[\"Custom_Lambert_Azimuthal\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ID[\"EPSG\",6326]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Lambert Azimuthal Equal Area\",\n            ID[\"EPSG\",9820]],\n        PARAMETER[\"Latitude of natural origin\",30,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-95,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",5500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",9500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
ext(bf)
#> SpatExtent : 4950000, 8250000, 9150000, 12150000 (xmin, xmax, ymin, ymax)
res(bf)
#> [1] 150000 150000
xres(bf)
#> [1] 150000
yres(bf)
#> [1] 150000
xmin(bf)
#> [1] 4950000
xmax(bf)
#> [1] 8250000
ymin(bf)
#> [1] 9150000
ymax(bf)
#> [1] 12150000
# }
```
