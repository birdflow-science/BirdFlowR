# Sample locations from a distribution

Probabilistically sample one or more locations from a set of
distributions.

## Usage

``` r
sample_distr(distr, n = 1, bf, format = "distr")
```

## Arguments

- distr:

  A vector representing a single distribution; a matrix representing one
  distribution per column, or (unlikely) an array in which the first
  dimension represents locations. The values in `distr` are treated as
  the relative probability of the species being in each position.

- n:

  Only used if `distr` is a vector representing a single model state, in
  which case that model state will be sampled `n` times to generate a
  matrix representing `n` sampled locations from that distribution.

- bf:

  A BirdFlow object, required if format is `"latlon"` or `"xy"`,
  optional otherwise.

- format:

  One of `"distr"` (the default), `"xy"`, `"latlon`, or `"i"` indicating
  what format the sample should be returned in.

## Value

One or more location samples from the distributions in `distr` the
format changes with the value of`format`:

- `distr`:

  Default. Return an object with the same dimensions as `distr` in which
  all the weight in each distribution in `distr` is assigned to a single
  location containing a 1 and remaining locations have 0's.

- `xy`:

  `x` and `y` coordinates of locations, usually as data frame but with
  3D input it will be an array.

- `latlon`:

  Return latitude (`lat`) and longitude (`lon`) coordinates in WGS 1984
  for the sampled locations, usually as a data frame but with 3D input
  it will be an array.

- `i`:

  Return location index for the sampled locations.

## Examples

``` r
bf <- BirdFlowModels::amewoo
d <- get_distr(bf, 5)

# default format "distr" returns an object similar to the input
# in which all the weight for each distribution has been concentrated
# in a single location
one_hot <- sample_distr(d)
all(one_hot %in% c(0, 1))
#> [1] TRUE
sum(one_hot)
#> [1] 1

# Sample 10 times from a single distribution and return x and y coordinates.
xy <- sample_distr(d,  10, format = "xy", bf = bf)
```
