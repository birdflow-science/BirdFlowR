# Evaluate BirdFlow model performance

DEPRECATED FUNCTION. Please use
[`distribution_performance()`](https://birdflow-science.github.io/BirdFlowR/reference/distribution_performance.md)
instead.

## Usage

``` r
evaluate_performance(x, distr_only = FALSE)
```

## Arguments

- x:

  A BirdFlow object

- distr_only:

  set to TRUE to calculate only `mean_distr_cor` and `min_distr_cor`
  metrics.

## Value

- mean_step_cor:

  Indicates on average how well the model project a single timestep. The
  mean correlation, across all timesteps, between the training (eBird
  S&T) distribution projected forward one timestep, and the training
  distribution for the projected timestep.

- min_step_cor:

  Indicates the quality of the worst single step projection. The minimum
  correlation (across all timesteps) between a single step projection
  each training distribution and the training distribution for the
  projected timestep.

- traverse_cor:

  Indicates how well does the model project through all timesteps. The
  correlation between last distribution projected iteratively from the
  fist training distribution; and the last training distribution.

- mean_distr_cor:

  Indicates on average how well the marginal preserves the training
  distributions. The mean correlation between the training distributions
  and distributions calculated from the marginals.

- min_distr_cor:

  Indicates how well the poorest marginal preserves the training
  distribution. The minimum observed correlation between a marginal and
  training distribution.

## Details

Calculate several the correlation between projected distributions and
the eBird Status and Trends (S&T) distributions used to train the
BirdFlow model.

"Training distribution" is used to describe the eBird S&T distributions
used to train the BirdFlow models. "Marginal distribution" describes a
distribution calculated from row or column sums of a marginal, joint
probability matrix. "Projected distribution" is used to describe a
training distribution that is multiplied with a transition matrix
(derived from the marginal distribution) to project forward one
timestep; or multiplied repeatedly with transition matrices to project
forward multiple timesteps.

Correlations are calculated for only the non-dynamically masked cells.
