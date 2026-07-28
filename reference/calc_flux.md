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

  :   The radius in meters around the points used to assess the
      detection rate for a movement at the point. With
      `method = "binary"`, if a point is within `radius` of the great
      circle line between two cell centers then the movement is detected
      at that point. For the two continuous detection methods there is a
      probability distribution for the location of the bird as it passes
      by the point, and the radius defines the band over which that
      probability is integrated, giving the detection rate.

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

  `method`

  :   The detection model used to determine how much of a transition's
      movement counts towards a point's BMTR:

      `"binary"`

      :   (default) Fast and deterministic. A movement line either does
          or does not pass within `radius` of the point, per
          [`is_between()`](https://birdflow-science.github.io/BirdFlowR/reference/is_between.md).

      `"continuous"`

      :   Assigns a continuous weight (0 to 1) based on the probability
          that a bird's actual path, modeled as spreading away from the
          straight line between two cells, passes within `radius` of the
          point. Uses planar (Euclidean) geometry in the model's native
          CRS, and is the recommended detection model when continuous
          weighting is desired.

      `"continuous-spherical"`

      :   The same continuous weighting as `"continuous"`, but computed
          with great-circle (spherical) geometry instead. Much slower,
          and not recommended for routine use; kept to allow assessing
          the impact of switching from spherical to Euclidean geometry.

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
