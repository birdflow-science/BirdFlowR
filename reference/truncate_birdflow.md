# Truncate the timespan of a BirdFlow model

`truncate_birdflow()` Eliminates marginals and/or transitions from a
BirdFlow model and adjusts other aspects of the model so that it only
covers part of a year. The intent is to reduce object size and
processing time when only part of the year is of interest.

## Usage

``` r
truncate_birdflow(bf, ...)
```

## Arguments

- bf:

  A BirdFlow object.

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

A BirdFlow model that only contains information about transitions for a
subset of the year as specified by `...`.

## Details

The model timesteps will always be numbered from 1 to
[`n_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
and so likely will not be consistent with weeks of the year with the
truncated model.

Currently it's possible to truncate both a fitted model and a model
produced by
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
but I have yet to create a way to export a preprocessed model after
truncating it (independently of
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md))
or to truncate during preprocessing. So currently the utility is limited
to reducing the size of fitted models.

It's possible to truncate a model over the year boundary but routes
generated from such a model will not plot correctly so it's not
recommended; see [issue
\#120](https://github.com/birdflow-science/BirdFlowR/issues/120).
