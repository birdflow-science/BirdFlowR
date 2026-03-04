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
