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

  Deprecated alternative to `x`. Previous versions of `lookup_date()`
  only supported timestep input and used `timestep` as the first
  argument.

## Value

A Date object

## Label dates

Each timestep (week) is labeled with its midpoint date. For example week
1 runs from January 1 to 7 and is labeled January 4, its midpoint.

Each transition or marginal spans from the midpoint of its first
timestep to the midpoint of its second, and is labeled with the midpoint
of *that* span. So transition `T_01-02`, which stretches from January 4
(midpoint and label for week 1) to January 11 (midpoint and label of
week 2), is labeled January 7 - the midpoint between the two.

## See also

[`get_dates()`](https://birdflow-science.github.io/BirdFlowR/reference/get_dates.md),
[`lookup_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep.md),
[`lookup_timestep_sequence()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
lookup_date(1:5, bf)
#> [1] "2023-01-04" "2023-01-11" "2023-01-18" "2023-01-25" "2023-02-01"
# }
```
