# Lookup a series of transitions connecting two dates or timesteps

The private function`lookup_transitions()` returns an ordered vector of
transition names that connect start to end. If `start` and `end` are
dates than their order determines whether the transitions flow forward
or backward in time. If they are timesteps than the `direction` argument
should be used to indicate whether to project "forward" or "backward" in
time possibly passing the year boundary.

## Usage

``` r
lookup_transitions(x, ...)
```

## Arguments

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

A character vector with the named transitions required to get between
`start` and `end`

## Details

Transitions are named "T\_\[from\]-\[to\]" where \[from\] and \[to\] are
timesteps padded with zeros. Direction is important; "T_03-04"
represents a transition backward in time.
