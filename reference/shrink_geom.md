# Shrink geometry component of a BirdFlow object

This is an internal helper function called twice by
[`shrink_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/shrink_birdflow.md)
it adjusts the `nrow`, `ncol`, `ext`, and `mask` elements of the `geom`
component of a BirdFlow model to the original extent, while preserving
the same number, location, and alignment of the unmasked cells - thus
nothing else in the object needs to change. If the geometry has not be
extended
([`shrink_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/shrink_birdflow.md),
[`extend_geom()`](https://birdflow-science.github.io/BirdFlowR/reference/extend_geom.md))
then it is returned as is.

## Usage

``` r
shrink_geom(geom)
```

## Arguments

- geom:

  The geometry component of a BirdFlow object

## Value

shrunk (or original) geometry
