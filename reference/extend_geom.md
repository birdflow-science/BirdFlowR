# Extend geometry component of a BirdFlow object

This is an internal helper function called twice by
[`extend_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/extend_birdflow.md)
it adjusts the `nrow`, `ncol`, `ext`, and `mask` elements of the `geom`
component of a BirdFlow model to expand the extent while preserving the
same number, location, and alignment of the unmasked cells - thus
nothing else in the object needs to change.

## Usage

``` r
extend_geom(geom, y)
```

## Arguments

- geom:

  The geometry component of a BirdFlow object

- y:

  An object that returns an extent when passed to
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html),
  this can be an extent, a SpatRaster, or a BirdFlow model.

## Value

extended geometry (covering larger area)
