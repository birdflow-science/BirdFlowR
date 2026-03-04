# Convert a BirdFlow object without a dynamic mask into one with a dynamic mask

A dynamic mask is a logical matrix of the same dimensions as the distr
matrix for `bf` holding TRUE for cells where the corresponding location
(row) and time (column) is included in the model.

## Usage

``` r
add_dynamic_mask(bf, dummy_mask = FALSE)
```

## Arguments

- bf:

  A BirdFlow object

- dummy_mask:

  If TRUE a mask is added to the object, but the mask is TRUE for every
  cell. This yields a BirdFlow object that works with the current mask
  dependent version of the package but mimics and old BirdFlow model.
  Note if the old model included state based sparsification the
  predictions should be identical even with `dummy_mask = FALSE` (the
  default).

## Value

A BirdFlow object that has a dynamic_mask component and in which the
marginals only includes transitions between cells that are not
dynamically masked.

## Examples

``` r
bf <- add_dynamic_mask(BirdFlowModels::amewoo)
```
