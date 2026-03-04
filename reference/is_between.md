# Determine if points are between BirdFlow cells

This internal function is used to create a betweenness array with
dimensions
[n_active(bf)](https://birdflow-science.github.io/BirdFlowR/reference/n_active()),
[n_active(bf)](https://birdflow-science.github.io/BirdFlowR/reference/n_active()),
and `length(points)`. The first two dimensions from and to cells of
possible connections between pairs of locations within the BirdFlow
model and both
[`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
elements. The third dimension represents `points` that might be between
each connection. Cell values are `TRUE` if the point is between the
associated model cells. Specifically, if the point is within `radius`
meters (along a great circle) of the great circle line connecting the
cell centers.

## Usage

``` r
is_between(
  bf,
  points = NULL,
  radius = NULL,
  n_directions = 1,
  skip_unconnected = TRUE,
  batch_size = 1e+05,
  check_radius = TRUE
)
```

## Arguments

- bf:

  A BirdFlow model

- points:

  The points to evaluate betweenness on. If NULL the cell centers of all
  the raster cells within the BirdFlow model that are between active
  cells in the model will be used. This is calculated by comparing the
  cell centers to a buffered convex hull around the active cell centers.

- radius:

  A point is considered between two locations if it is within `radius`
  meters (along a great circle) of the great circle line between the
  locations. `radius` defaults to half the cell size
  (`mean(res(bf))/2`).

- n_directions:

  The number of (equally spaced) directional bins to classify bearings
  into. Currently only `1` is supported.

- skip_unconnected:

  If `TRUE` then only connections that exist in `bf` will be evaluated,
  and between matrix will erroneously indicate that no points are
  between locations that are not connected. The resulting array can
  still be used with the model it was built for because those missing
  connections would always have zero probability.

- batch_size:

  controls the number of movement lines that are processed at a time. A
  smaller `batch_size` will conserve memory at a slight performance
  cost. The number of batches will be less than or equal to
  `n_active(bf)^2 / batch_size`.

- check_radius:

  If `TRUE` an error will be thrown if the radius is not between the
  resolution and 1/4 the resolution of `bf`. Outside of that range the
  algorithm is likely to yield distorted results. `0.5 * mean(res(bf))`
  is the default, and recommended radius.

## Value

A list with:

- between:

  An array with dimensions representing the "from" location, the "to"
  location, and the `points`. Cells are `TRUE` if the point is between
  the associated from and to locations.

- points:

  A data,frame of points that define the third dimension in `between`.
  It is identical to the input `points` if they are not `NULL`.
  Otherwise it will be a data frame with columns `x`, `y`, and `i`
  corresponding to the third dimension in `between`. `i` will be `NA`
  for points that are not within the mask but fall between active cells.

- radius:

  The radius of the circle in meters.

## Details

If `points` and `radius` are `NULL` they default to the cell radius and
the center of all cells within the BirdFlow extent that fall between any
active cells. This includes all cell centers within a convex hull (in
spherical coordinates) around the active cells in `bf`.
