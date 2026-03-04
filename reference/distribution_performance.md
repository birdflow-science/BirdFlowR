# Evaluate a BirdFlow models ability to recreate training distributions

Calculate the correlation between model derived distributions and the
eBird Status and Trend distributions used to train the BirdFlow model.

## Usage

``` r
distribution_performance(x, metrics = NULL, ...)
```

## Arguments

- x:

  A BirdFlow object

- metrics:

  If NULL calculate all metrics. Otherwise set to a subset of the metric
  names to calculate only those metrics.

- ...:

  Arguments passed on to
  [`lookup_timestep_sequence`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)

  `season`

  :   a season name, season alias, or "all". See
      [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
      for options.

  `start`

  :   The starting point in time specified as a timestep, character
      date, or date object.

  `end`

  :   The ending point in time as a date or timestep.

  `direction`

  :   Either "forward" or "backward" defaults to `"forward"` if not
      processing dates. If using date input `direction` is optional and
      is only used to verify the direction implicit in the dates.

  `season_buffer`

  :   Only used with `season` input. `season_buffer` is passed to
      [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
      and defaults to 1; it is the number of timesteps to extend the
      season by at each end.

  `n_steps`

  :   Alternative to `end` The end will be `n_steps` away from `start`
      in `direction`; and the resulting sequence will have `n_step`
      transitions and `n_steps + 1` timesteps.

## Value

- mean_step_cor:

  Indicating on average how well the model projects a single timestep,
  `mean_step_cor` is the mean correlation, across all timesteps, between
  the training (eBird Status and Trends) distribution projected forward
  one timestep, and the training distribution for that projected
  timestep.

- min_step_cor:

  Indicates the quality of the worst single step projection. The minimum
  correlation (across all timesteps) between a single step projection of
  the training distribution and the training distribution for the
  projected timestep.

- mean_distr_cor:

  Indicates on average how well the marginal preserves the training
  distributions. The mean correlation between the training distributions
  and distributions calculated from the marginals.

- min_distr_cor:

  Indicates how well the poorest marginal preserves the training
  distribution. The minimum observed correlation between a marginal and
  training distribution.

- st_traverse_cor, md_traverse_cor:

  Indicates how well the model projects a distribution through multiple
  timesteps. They are the correlation between last distribution in a
  series projected iteratively forward from the fist distribution; and
  the eBird Status and Trends (training) distribution for the last
  timestep. `st_traverse_cor` starts with the first timestep Status and
  Trends (st) distribution, while `md_traverse_cor` starts with a the
  marginal distribution for the same timestep; both versions compare the
  projected distribution to the eBird Status and Trends distribution for
  same timestep.

- n_states:

  The total number of non_zero values across all marginal derived
  distributions in the model. These are the number of states (locations
  in space and time) that can be reached in the model.

## Details

BirdFlow models are trained on eBird Status and Trends distributions
("Training distributions"). "Marginal distribution" describes a
distribution calculated from row or column sums of a marginal, joint
probability matrix (also part of the model). "Projected distribution" is
used to describe a distribution that is multiplied with a transition
matrix (derived from the marginal distribution) to project forward one
timestep or multiplied repeatedly with a series of transition matrices
to project forward multiple timesteps

Correlations are calculated for only the non-dynamically masked cells.

The `...` argument can be used to define a subset of time to evaluate
over in which case all metrics will be calculated only on that subset.
The default is to use all timesteps.

## See also

[`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md)
to evaluate a BirdFlow model using movement data from real birds.

## Examples

``` r
 bf <- BirdFlowModels::amewoo

if (FALSE) { # \dontrun{
# full model - skipping because it's slow
distribution_performance(bf)
} # }
# Just for prebreeding_migration
distribution_performance(bf, season = "prebreeding_migration")
#> $min_step_cor
#> [1] 0.9830113
#> 
#> $mean_step_cor
#> [1] 0.9935296
#> 
#> $min_distr_cor
#> [1] 0.9804372
#> 
#> $mean_distr_cor
#> [1] 0.9920057
#> 
#> $st_traverse_cor
#> [1] 0.9845607
#> 
#> $md_traverse_cor
#> [1] 0.9907496
#> 
#> $n_states
#> [1] 3138
#> 
```
