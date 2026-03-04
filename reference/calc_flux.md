# Calculate Bird Flow Migration Traffic Rate

DEPRECATED FUNCTION. Please use
[`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)
instead.

## Usage

``` r
calc_flux(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`calc_bmtr`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)

  `bf`

  :   A BirdFlow model

  `points`

  :   A set of points to calculate movement through. If `points` is
      `NULL` they will default to the BirdFlow model cells that are
      either active or fall between two active cells. Otherwise a data
      frame with `x` and `y` columns containing point coordinates in
      [crs(bf)](https://rspatial.github.io/terra/reference/crs.html).

  `radius`

  :   The radius in meters around the points used to assess whether a
      movement line passes by (or through) the point. If a point is
      farther than `radius` from a great circle line between two cells
      centers then it is not between them.

  `n_directions`

  :   The number of directional bins to use for recording movement
      direction. Must be either `1` indicating no direction information
      or an even number. This is a placeholder, currently only `1` is
      supported.

  `format`

  :   The format to return the results in one of:

      `"points"`

      :   Returns a list with `bmtr` a matrix or array of bmtr values,
          and `points` a data frame of either the input `points` or the
          default cell center derived points.

      `"dataframe"`

      :   Returns a "long" data frame with columns:

          - `x` and `y` coordinates of the points.

          - `transition` Transition code.

          - `bmtr` The bmtr at the point. See "Units" below .

          - `date` The date associated with the transition, will be at
            the midpoint between timesteps.

      `"SpatRaster"`

      :   Returns a
          [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
          with layers for each transition.

  `weighted`

  :   If `FALSE` use the original and quicker version of bmtr that sums
      all the marginal probability for transitions that pass within a
      fixed distance of the point. If `TRUE` assign a weight to the
      point and transition combo that then is multiplied by the marginal
      probability before summing. This argument is experimental but the
      default value is identical to the old version. The argument name
      and behavior when set to `TRUE` may change.

  `batch_size`

  :   controls the number of movement lines that are processed at a
      time. A smaller `batch_size` will conserve memory at a slight
      performance cost. The number of batches will be less than or equal
      to `n_active(bf)^2 / batch_size`.

  `check_radius`

  :   If `TRUE` an error will be thrown if the radius is not between the
      resolution and 1/4 the resolution of `bf`. Outside of that range
      the algorithm is likely to yield distorted results.
      `0.5 * mean(res(bf))` is the default, and recommended radius.

## Value

See `format` argument.
