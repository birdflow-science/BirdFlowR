# calculate the weights of transitions for bmtr points

`calc_dist_weights()` is an internal function that takes summary stats
on the relationship between points and a transition line and returns the
weight that should be used for that transition.

## Usage

``` r
calc_dist_weights(
  dist_to_line,
  dist_along_line,
  line_lengths,
  radius_m,
  res_m,
  kernel = c("m3", "bb", "m1", "m5", "sq"),
  gamma = 3e+10,
  kl = 9e+05,
  s1 = 200
)
```

## Arguments

- dist_to_line:

  How far is the point from the line (m)

- dist_along_line:

  How far along the line is the point, after projecting it onto the line
  (m)

- line_lengths:

  How long is the line (m)

- radius_m:

  The radius of the transect at the bmtr points - used to determine the
  band of probability density that will be added to form the weight.

- res_m:

  The resolution of the associated bird flow model, used to determine
  the nugget added to the variance to represent the uncertainty in the
  starting and ending location of the transition.

- kernel:

  The kernel used for calculating the standard deviation in the
  probability distribution:

  `"m3"`

  :   (default) Matern 3/2. Driven by `gamma` and `kl`.

  `"m1"`

  :   Matern 1/2. Driven by `gamma` and `kl`.

  `"m5"`

  :   Matern 5/2. Driven by `gamma` and `kl`.

  `"sq"`

  :   Squared-exponential (Gaussian). Driven by `gamma` and `kl`.

  `"bb"`

  :   Brownian bridge. Driven by `s1`.

  See
  [`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md)
  to explore how these kernels and their hyperparameters shape the
  spread.

- gamma:

  Spread magnitude hyperparameter (m^2, a variance) for the
  Matern-family and squared-exponential kernels (`"m1"`, `"m3"`, `"m5"`,
  `"sq"`). Ignored for `kernel = "bb"`. The default was tuned by eye
  with
  [`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md).

- kl:

  Lengthscale hyperparameter (m) for the Matern-family and
  squared-exponential kernels (`"m1"`, `"m3"`, `"m5"`, `"sq"`). Ignored
  for `kernel = "bb"`. The default was tuned by eye using
  [`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md).

- s1:

  Spread magnitude hyperparameter (units of sqrt(m), not m) for the
  `"bb"` kernel. Ignored for all other kernels. The default was tuned by
  eye against
  [`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md).

## Value

A vector of weights of the same length as the first three arguments.

## Details

The first three arguments can all be vectors in which case the
calculations will be vectorized over the corresponding elements.

This is a preliminary version of the function and will likely change.

## See also

[`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md)
