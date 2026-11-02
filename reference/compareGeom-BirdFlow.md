# BirdFlow compareGeom methods

These are methods for
[`terra::compareGeom()`](https://rspatial.github.io/terra/reference/compareGeom.html)
that work when one or both of arguments are BirdFlow objects.

## Usage

``` r
# S4 method for class 'BirdFlow,BirdFlow'
compareGeom(x, y, ...)

# S4 method for class 'SpatRaster,BirdFlow'
compareGeom(x, y, ...)

# S4 method for class 'BirdFlow,SpatRaster'
compareGeom(x, y, ...)
```

## Arguments

- x:

  A BirdFlow or SpatRaster object

- y:

  A BirdFlow or SpatRaster object

- ...:

  Arguments passed on to
  [`terra::compareGeom`](https://rspatial.github.io/terra/reference/compareGeom.html)

  :   

## Value

As with
[`terra::compareGeom()`](https://rspatial.github.io/terra/reference/compareGeom.html):
a logical scalar, `TRUE` if the geometries match. If they don't match,
and `stopOnError` (passed via `...`) is `TRUE` (the default), execution
stops with an error instead of returning `FALSE`.

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
r <- rast(bf, which = 1)
terra::compareGeom(bf, bf)
#> [1] TRUE
terra::compareGeom(r, bf)
#> [1] TRUE
terra::compareGeom(bf, r)
#> [1] TRUE
# }
```
