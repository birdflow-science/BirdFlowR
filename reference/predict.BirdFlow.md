# Predict bird distributions

`predict()` projects bird distributions into the future or past. Given
an initial distribution and time period specified via `...`, `predict()`
generates probability distributions for each timestep.

## Usage

``` r
# S3 method for class 'BirdFlow'
predict(object, distr, ...)
```

## Arguments

- object:

  A BirdFlow model object.

- distr:

  A starting distribution.

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

If multiple starting distributions are input in a matrix the result will
be an array with dimensions: location, distribution, and time. With one
input distribution the result will be a matrix with dimensions: location
and time.

## See also

- [`lookup_timestep_sequence()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)
  processes the time inputs (`start`, `end`, `direction`, and
  `season_buffer`)

- [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
  and
  [`route_migration()`](https://birdflow-science.github.io/BirdFlowR/reference/route_migration.md)
  are similar to `predict()` but generate routes instead of
  distributions.
