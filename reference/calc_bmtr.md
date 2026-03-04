# Estimate BirdFlow Migration Traffic Rate (BMTR)

`calc_bmtr()` estimates the proportion of the species that passes near a
set of points during each transition in a BirdFlow model.

## Usage

``` r
calc_bmtr(
  bf,
  points = NULL,
  radius = NULL,
  n_directions = 1,
  format = NULL,
  batch_size = 5e+05,
  check_radius = TRUE,
  weighted = FALSE
)
```

## Arguments

- bf:

  A BirdFlow model

- points:

  A set of points to calculate movement through. If `points` is `NULL`
  they will default to the BirdFlow model cells that are either active
  or fall between two active cells. Otherwise a data frame with `x` and
  `y` columns containing point coordinates in
  [crs(bf)](https://rspatial.github.io/terra/reference/crs.html).

- radius:

  The radius in meters around the points used to assess whether a
  movement line passes by (or through) the point. If a point is farther
  than `radius` from a great circle line between two cells centers then
  it is not between them.

- n_directions:

  The number of directional bins to use for recording movement
  direction. Must be either `1` indicating no direction information or
  an even number. This is a placeholder, currently only `1` is
  supported.

- format:

  The format to return the results in one of:

  `"points"`

  :   Returns a list with `bmtr` a matrix or array of bmtr values, and
      `points` a data frame of either the input `points` or the default
      cell center derived points.

  `"dataframe"`

  :   Returns a "long" data frame with columns:

      - `x` and `y` coordinates of the points.

      - `transition` Transition code.

      - `bmtr` The bmtr at the point. See "Units" below .

      - `date` The date associated with the transition, will be at the
        midpoint between timesteps.

  `"SpatRaster"`

  :   Returns a
      [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
      with layers for each transition.

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

- weighted:

  If `FALSE` use the original and quicker version of bmtr that sums all
  the marginal probability for transitions that pass within a fixed
  distance of the point. If `TRUE` assign a weight to the point and
  transition combo that then is multiplied by the marginal probability
  before summing. This argument is experimental but the default value is
  identical to the old version. The argument name and behavior when set
  to `TRUE` may change.

## Value

See `format` argument.

## Units

The total relative abundance passing through the circle centered on the
point is divided by the diameter of the circle in kilometers. The units
of the returned value is therefore roughly the proportion (\\P\\) of the
species's population that is expected to pass through each km of a line
oriented perpendicular to the movement at each point: \\\frac{P}{km
\cdot week}\\

Multiplying the result by the total population would yield:
\\\frac{birds}{km \cdot week}\\

## Limitations

`calc_bmtr()` makes the incorrect simplifying assumption that birds
follow the shortest (great circle) path between the center of the the
source and destination raster cells. Caution should be used when
interpreting the results especially around major geographic features
such as coasts, large lakes, mountain ranges, and ecological system
boundaries that might result in non-linear migration paths.

`calc_bmtr()` assumes that a line passes by a point if any part of the
line is within the radius of the point. This assumption breaks down if
the radius is much larger than the movement lengths as points that are
ahead of the line may still be within a radius of the line. In the
extreme a large enough radius on a point outside of the entire range
will capture all the movement. This isn't a problem with the default
points and radius as the points ahead of the line will never be within
the radius.

The default points for `calc_bmtr()` are aligned with the cell centers
as are the movement lines. This alignment means that a very small radius
will result in an overestimate of bmtr. The default value of half the
cell size is sufficient for this not to be a problem, as we are
capturing and standardizing the units based on the entire cell area that
that point represents.

## Examples

``` r
if (FALSE) { # \dontrun{
bf <- BirdFlowModels::amewoo
bmtr <- calc_bmtr(bf)

plot_bmtr(bmtr, bf)

animate_bmtr(bmtr, bf)
} # }
```
