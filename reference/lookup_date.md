# Retrieve date associated with timesteps, transitions, or marginals

Retrieve date associated with timesteps, transitions, or marginals

## Usage

``` r
lookup_date(x, bf, timestep = NULL)
```

## Arguments

- x:

  A vector of one of the following formats:

  1.  Integer between 1 and `n_timesteps(bf)` representing timestep.

  2.  Character with "T" followed by digits that indicate timesteps,
      (this format is used internally to label timestep dimensions of
      objects)

  3.  Marginal or Transition names. These start with either "T\_" or
      "M\_", and then have two timesteps represented by digits and
      separated by a dash, E.g. "T_01-02".

- bf:

  A BirdFlow object

- timestep:

  Deprecated alternative to `x`. Previous versions of `lookup_dates()`
  only supported timestep input and used `timestep` as the first
  argument.

## Value

A Date object

## See also

[`get_dates()`](https://birdflow-science.github.io/BirdFlowR/reference/get_dates.md),
[`lookup_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep.md),
[`lookup_timestep_sequence()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)

## Examples

``` r
bf <- BirdFlowModels::amewoo
lookup_date(1:5, bf)
#> [1] "2021-01-04" "2021-01-11" "2021-01-18" "2021-01-25" "2021-02-01"
```
