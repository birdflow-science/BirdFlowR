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
  method = c("binary", "continuous", "continuous-spherical"),
  ...
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

  The radius in meters around the points used to assess the detection
  rate for a movement at the point. With `method = "binary"`, if a point
  is within `radius` of the great circle line between two cell centers
  then the movement is detected at that point. For the two continuous
  detection methods there is a probability distribution for the location
  of the bird as it passes by the point, and the radius defines the band
  over which that probability is integrated, giving the detection rate.

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

- method:

  The detection model used to determine how much of a transition's
  movement counts towards a point's BMTR:

  `"binary"`

  :   (default) Fast and deterministic. A movement line either does or
      does not pass within `radius` of the point, per
      [`is_between()`](https://birdflow-science.github.io/BirdFlowR/reference/is_between.md).

  `"continuous"`

  :   Assigns a continuous weight (0 to 1) based on the probability that
      a bird's actual path, modeled as spreading away from the straight
      line between two cells, passes within `radius` of the point. Uses
      planar (Euclidean) geometry in the model's native CRS, and is the
      recommended detection model when continuous weighting is desired.

  `"continuous-spherical"`

  :   The same continuous weighting as `"continuous"`, but computed with
      great-circle (spherical) geometry instead. Much slower, and not
      recommended for routine use; kept to allow assessing the impact of
      switching from spherical to Euclidean geometry.

- ...:

  For `method = "continuous"` or `"continuous-spherical"`, additional
  arguments forwarded to
  [`calc_dist_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_dist_weights.md)
  to control the spread kernel used to model uncertainty in a bird's
  path: `kernel`, `gamma`, `kl`, and `s1`. See
  [`calc_dist_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_dist_weights.md)
  for the full list of supported kernels and their hyperparameters, and
  [`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md)
  to explore how they shape the spread before running this (potentially
  expensive) function. Not applicable, and an error, for
  `method = "binary"`.

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

`calc_bmtr()` makes an incorrect simplifying assumption about the path
birds take: either that they follow the shortest path between the
centers of the source and destination raster cells
(`method = "binary"`), or that they are distributed symmetrically in a
Gaussian distribution around that path (the two continuous methods).
That path is a great circle in all cases except `method = "continuous"`,
which uses a straight line in the model's projected CRS. Caution should
be used when interpreting the results especially around major geographic
features such as coasts, large lakes, mountain ranges, and ecological
system boundaries that might result in non-linear migration paths.

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

## See also

[`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md)
to explore the spread kernel hyperparameters accepted via `...`.

## Examples

``` r

if (FALSE) { # \dontrun{
bf <- BirdFlowModels::amewoo

# Binary detection along shortest path
bmtr <- calc_bmtr(bf)
plot_bmtr(bmtr, bf)

animate_bmtr(bmtr, bf)

# Continuous detection with a wider spread kernel
# (gamma defaults to 3e10)
bmtr2 <- calc_bmtr(bf, method = "continuous", gamma = 6e10)
animate_bmtr(bmtr2, bf)

# Visualize spread for the continuous detection with wider spread
visualize_distance_weights(line_lengths = c(2, 5, 10) * xres(bf),
                           gamma = 6e10, res_m = xres(bf))

# Visualize spread for the continuous detection with default values
visualize_distance_weights(line_lengths = c(2, 5, 10) * xres(bf),
                           res_m = xres(bf))

} # }



```
