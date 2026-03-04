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
  method = "m3"
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

- method:

  The method used for calculating the standard deviation in the
  probability distribution. Currently `"m3"`, Martern 3/2; and `"bb"`,
  Brownian bridge are supported.

## Value

A vector of weights of the same length as the first three arguments.

## Details

The first three arguments can all be vectors in which case the
calculations will be vectorized over the corresponding elements.

This is a preliminary version of the function and will likely change.
