# Calculate log likelihoods of observed bird movements

`interval_log_likelihood()` calculates the log likelihoods of inferred
bird movement based on two observation points (in time and space). The
second point must have a different timestep (week) than the first, but
the location can remain the same. Note `interval_log_likelihood()`
predates the `BirdFlowIntervals` class and so is now deprecated. If you
are thinking of using this function in new code please consider
[`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md),
[`as_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md),
[`as_BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowIntervals.md),
and
[`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md)
to make `BirdFlowIntervals` and then calculate a full suite of metrics
including log likelihood.

## Usage

``` r
interval_log_likelihood(intervals, observations, bf, one_at_a_time = FALSE)
```

## Arguments

- intervals:

  A data.frame that describes intervals (movements or stationary
  periods) for which log likelihood will be calculated by referencing
  the `id` column in `observations`.

  `from`

  :   Observation ID of the starting location and date.

  `to`

  :   Observation ID of the ending location and date.

  ...

  :   Any additional columns will be included in the returned object but
      not used by this function. It probably should include an interval
      ID.

- observations:

  A data.frame describing observations of birds each row should be an
  individual bird, at a location, and date.

  `id`

  :   Unique observation identifier.

  `lon` , `lat`

  :   Longitude and latitude of observation in WGS84 (EPSG:4326)

  `date`

  :   Date associated with observation. See
      [`lookup_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep.md)
      for valid formats.

  ...

  :   Other columns allowed, but will be ignored.

- bf:

  A BirdFlow object.

- one_at_a_time:

  Mainly here for debugging. If FALSE, the default, then all intervals
  that start at the same timestep are processed together, otherwise each
  interval is processed separately. Results should be identical, TRUE
  uses less memory but is slower.

## Value

The intervals table is returned along with new columns:

- log_likelihood :

  The model derived log likelihood of the interval.

- null_ll :

  The log likelihood of the interval based on a null model that assumes
  the eBird S&T distribution for the species at the end point.

- lag :

  The number of timesteps (likely weeks) between the start and end of
  the interval.

- exclude :

  TRUE if the log likelihood couldn't be calculated for the interval, in
  which case there should also be a TRUE in one of the remaining columns
  indicating why.

- not_active :

  If TRUE the start or end point is not within the model mask

- dynamic_mask :

  If TRUE eBird S&T has assigned zero probability to the the start or
  end point for the associated date and therefore it is excluded by the
  dynamic mask or state based sparsification.

- sparse :

  TRUE if the model assigned zero probability to the interval and it
  wasn't due to any of the other reasons. This is likely due to
  sparsification eliminating all possible routes between the start and
  end point.

- same_timestep :

  TRUE if the start and end timesteps are the same, a lag of zero.

- bad_date :

  TRUE if the date couldn't be parsed, or if `bf` is a partial model and
  the date falls in the uncovered portion of the year.

The returned table rows will have a 1:1 correspondence with the input
`intervals` table.

## Details

The core of this function is calling
[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)
on a distribution that has the starting location hot (value of 1) and
all other locations zero and then extracting the probability of the
ending week and location. The log of this probability is returned in the
`log_likelihood` column. The null model assumes that the eBird S&T
distribution and thus the `null_ll` column contains the log of the
probability density from the S&T distribution at the ending week and
location.

The observations and intervals are separated into two tables to allow
flexibility in assigning and evaluating intervals. With tracking data in
which the frequency of observations is much greater than the weekly S&T
data there are a lot of choices to be made and this function leaves
those decisions to the user.

## See also

[`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md)

## Examples

``` r
bf <- BirdFlowModels::rewbla
observations <- BirdFlowModels::rewbla_observations
intervals <- BirdFlowModels::rewbla_intervals
intervals <- intervals[1:20, ] # toy example
intervals <- interval_log_likelihood(intervals, observations, bf)
head(intervals, 3)
#>           bird_id  from    to interval_id log_likelihood   null_ll lag exclude
#> 1017 B19221769152  2223  2224        1017      -3.157935 -4.453952  47   FALSE
#> 4775 B77724064244 10149 10150        4775      -3.429942 -4.870969  50   FALSE
#> 2177 B29252852992  4625  4626        2177      -5.135462 -5.816720  21   FALSE
#>      not_active dynamic_mask sparse same_timestep bad_date
#> 1017      FALSE        FALSE  FALSE         FALSE    FALSE
#> 4775      FALSE        FALSE  FALSE         FALSE    FALSE
#> 2177      FALSE        FALSE  FALSE         FALSE    FALSE
```
