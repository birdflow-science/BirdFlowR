# Validate geom component of a BirdFlow or related object

This is called from `validate_BirdFlow`, as well as some of the route
and interval validation functions

## Usage

``` r
validate_geom(geom, n_active, throw_error = TRUE)
```

## Arguments

- geom:

  A geom list

- n_active:

  The number of active cells in the model see
  [`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  also stored in `bf$metadat$n_active`

- throw_error:

  If `TRUE` throw errors that are found. IF false return a problem data
  frame. throw_error = FALSE is for compatibility with
  `validate_BirdFlow`

## Value

a problem data frame
