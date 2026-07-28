# Assign weights to points based on how close they are to the planar (Euclidean) line associated with each transition

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
calc_euclidean_detection_rate(
  bf,
  points = NULL,
  radius = NULL,
  n_directions = 1,
  skip_unconnected = TRUE,
  batch_size = 1e+05,
  check_radius = TRUE,
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

- check_radius:

  If `TRUE` an error will be thrown if the radius is not between the
  resolution and 1/4 the resolution of `bf`. Outside of that range the
  algorithm is likely to yield distorted results. `0.5 * mean(res(bf))`
  is the default, and recommended radius.

- ...:

  Additional arguments forwarded to
  [`calc_dist_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_dist_weights.md),
  allowing the spread kernel (`kernel`) and its hyperparameters
  (`gamma`, `kl`, `s1`) to be tuned.

## Value

A list with:

- between:

  An array with dimensions representing the "from" location, the "to"
  location, and the `points`. Cells are weights and will be non-zero if
  the radius around the point intersects 1.96 standard deviations of the
  normal distribution of probabilities around the line.

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
all cell centers within a convex hull (in the model's native, projected
CRS) around the active cells in `bf` and thus is almost always more than
just the active cells.

Unlike
[`calc_spherical_detection_rate()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_spherical_detection_rate.md),
this projects points onto the line segment connecting each pair of
active cells directly in the model's native (planar) CRS, using
vectorized matrix algebra
([`project_points_simple()`](https://birdflow-science.github.io/BirdFlowR/reference/project_points_simple.md))
rather than great-circle math. This is substantially faster and is the
recommended continuous detection method.

## See also

[`is_between()`](https://birdflow-science.github.io/BirdFlowR/reference/is_between.md),
[`calc_spherical_detection_rate()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_spherical_detection_rate.md),
and
[`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)
