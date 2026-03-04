# Calculate interval metrics for a single interval

This internal function evaluates model performance using as single
interval which is a pair of observations of a real bird separated by at
least a week. This is a helper function for `[calc_interval_metrics()`

## Usage

``` r
calc_single_interval_metrics(birdflow_interval_row, bf, gcd, st_dists)
```

## Arguments

- birdflow_interval_row:

  A row of data in the `BirdFlowIntervals` object

- bf:

  BirdFlow model

- gcd:

  Matrix of great circle distance

- st_dists:

  Matrix of S&T distribution with weeks as columns, location as rows,
  probability as values.

## Value

A named vector with various metrics

- pred:

  Weighted average great-circle distance (km) from the BF prediction
  distribution to the actual encounter cell

- st:

  Weighted average great-circle distance (km) from the S&T empirical
  distribution to the actual encounter cell

- win_prob:

  Probability that BF is closer than S&T (i.e.“win” probability of BF
  vs. S&T)

- win_distance:

  Absolute distance improvement (km): `st – pred`

- win_distance_fraction:

  Normalized distance improvement: `(st – pred) / st`

- global_prob_of_the_starting:

  Probability (relative abundance) of the starting cell in the BF
  distribution at the start date

- elapsed_days:

  Elapsed time of the interval (days) between banding (`date1`) and
  encounter (`date2`)

- elapsed_km:

  Observed great-circle distance (km) between banding and encounter
  locations

- null_ll:

  Log-likelihood of the encounter cell under the S&T distribution:
  `log(final_st_distr[i_final])`

- ll:

  Log-likelihood of the encounter cell under the BF prediction:
  `log(preds_final[i_final])`

- energy_score_bf:

  Energy score of the BF predictive distribution (with \\\beta=1\\)

- energy_score_st:

  Energy score of the S&T empirical distribution (with \\\beta=1\\)

- energy_improvement:

  Difference in energy score: `energy_score_st – energy_score_bf`

- pred_elapsed_dist_by_pred:

  Predicted elapsed distance (km) from starting cell, weighted by BF
  predictions

- pred_elapsed_dist_by_st:

  Predicted elapsed distance (km) from starting cell, weighted by S&T
  distribution

## See also

[`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md)
