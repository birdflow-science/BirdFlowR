# Assign weights to points based on how close they are to the great circle line associated with each transition

This internal function is used to create a betweenness weights array
with dimensions
[n_active(bf)](https://birdflow-science.github.io/BirdFlowR/reference/n_active()),
[n_active(bf)](https://birdflow-science.github.io/BirdFlowR/reference/n_active()),
and `length(points)`. The first two dimensions represent from and to
cells of possible connections between pairs of locations within the
BirdFlow model and both have
[`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
elements. The third dimension represents reference points that might be
between each connection. Cell values are the weight to use when adding
the transition probabilities to the reference point BMTR

## Usage

``` r
weight_between(
  bf,
  weight_fun = NULL,
  points = NULL,
  radius = NULL,
  n_directions = 1,
  skip_unconnected = TRUE,
  batch_size = 1e+05,
  ...
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

  The probability density along a transect perpendicular to the line and
  intersecting the point is summed over the interval within `radius`
  meters of the point. `radius` defaults to half the cell size
  (`mean(res(bf))/2`).

- n_directions:

  The number of (equally spaced) directional bins to classify bearings
  into. Currently only `1` is supported.

- skip_unconnected:

  If `TRUE` then only connections that exist in `bf` will be evaluated,
  and between matrix will erroneously indicate that the weights
  associated with transitions that aren't used is always 0. The
  resulting array can still be used with the model it was built for
  because those missing connections would always have zero probability.

- batch_size:

  controls the number of movement lines that are processed at a time. A
  smaller `batch_size` will conserve memory at a slight performance
  cost. The number of batches will be less than or equal to
  `n_active(bf)^2 / batch_size`.

## Value

A list with:

- between:

  An array with dimensions representing the "from" location, the "to"
  location, and the `points`. Cells are weights and will be non-zero if
  the radius around the point intersects 1.96 standard deviations of the
  normal distribution of probabilities around the great circle line.

- points:

  A data,frame of points that define the third dimension in `between`.
  It is identical to the input `points` if they are not `NULL`.
  Otherwise it will be a data frame with columns `x`, `y`, and `i`
  corresponding to the third dimension in `between`. `i` will be `NA`
  for points that are not within the mask but fall between active cells.

- radius:

  The radius of the circle in meters.

## Details

If `points` are `NULL` they default to the center of all cells within
the BirdFlow extent that fall between any active cells. This includes
all cell centers within a convex hull (in spherical coordinates) around
the active cells in `bf` and thus is almost always more than just the
active cells.

`weight_between()` and
[`is_between()`](https://birdflow-science.github.io/BirdFlowR/reference/is_between.md)
should only differ slightly in their results when calculated on the same
model.

1, The number of included reference points may differ. 2. The
betweenness array will be real weights from 0 to 1 with
`weight_between()` and logical with
[`is_between()`](https://birdflow-science.github.io/BirdFlowR/reference/is_between.md)

## See also

[`is_between()`](https://birdflow-science.github.io/BirdFlowR/reference/is_between.md)
and
[`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)
