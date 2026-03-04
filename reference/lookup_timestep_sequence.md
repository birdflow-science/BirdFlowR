# Lookup a sequence of timesteps

`lookup_timestep_sequence()` returns an ordered vector of timesteps,
possibly crossing over the year boundary.

## Usage

``` r
lookup_timestep_sequence(
  x,
  season = NULL,
  start = NULL,
  end = NULL,
  direction = NULL,
  season_buffer = 1,
  n_steps = NULL
)
```

## Arguments

- x:

  A BirdFlow object

- season:

  a season name, season alias, or "all". See
  [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
  for options.

- start:

  The starting point in time specified as a timestep, character date, or
  date object.

- end:

  The ending point in time as a date or timestep.

- direction:

  Either "forward" or "backward" defaults to `"forward"` if not
  processing dates. If using date input `direction` is optional and is
  only used to verify the direction implicit in the dates.

- season_buffer:

  Only used with `season` input. `season_buffer` is passed to
  [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
  and defaults to 1; it is the number of timesteps to extend the season
  by at each end.

- n_steps:

  Alternative to `end` The end will be `n_steps` away from `start` in
  `direction`; and the resulting sequence will have `n_step` transitions
  and `n_steps + 1` timesteps.

## Value

An integer sequence of timesteps.

## Details

`lookup_timestep_sequence()` is unlikely to be called directly but it's
arguments will likely be passed from other functions like
[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)
and
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md).

Whether called directly or via another function
`lookup_timestep_sequence()` is a flexible function that allows several
ways of defining the sequence.

1.  **Dates**. Input character dates (e.g. "2023-06-21") or date objects
    to both `start` and `end`. The direction will be determined from the
    dates so `direction` is optional. If `direction` is used an error
    will be thrown if it doesn't conform with the direction implicit in
    the dates.

2.  **Timesteps**. Use numeric `start` and `end` to indicate a starting
    and ending timestep. Since many models are circular `direction` is
    used to determine whether to go forward or backwards from `start` to
    `end`; `direction` will default to forward.

3.  **Season**. Input a season name (or alias) into `season`. `"all"`
    can also be used to indicate all timesteps, in which case with
    cyclical models the last timestep will match the first. If `season`
    is used (and isn't `"all`) `season_bufffer` indicates the number of
    timesteps to extend the season by. The default of 1 means that the
    sequence will start 1 timestep (week) before and end 1 timestep
    after the dates for the season returned by `[species_info()]`.
    `direction` is followed and defaults to forward.

4.  **Start and offset**. Use `start` with a timestep or date input and
    `n_steps` to create a sequence that starts at `start` and then
    proceeds `n_steps` in `direction` which default to "forward". The
    returned object will have `n_steps + 1` timesteps in the sequence.

5.  **Default** If `season` and `start` are both NULL (or omitted) the
    default is to return all timesteps in the model, equivalent to
    `season = "all".`

## Examples

``` r
bf <- BirdFlowModels::rewbla

# 1. Dates - order of dates determines direction
lookup_timestep_sequence(bf, start = "2023-12-1", end = "2024-01-20")
#> [1] 48 49 50 51 52  1  2  3
lookup_timestep_sequence(bf, start = "2024-01-20", end = "2023-12-1")
#> [1]  3  2  1 52 51 50 49 48

# 2. Timesteps - direction defaults to "forward"
lookup_timestep_sequence(bf, start = 50, end = 3)
#> [1] 50 51 52  1  2  3
lookup_timestep_sequence(bf, start = 50, end = 3, direction = "backward")
#>  [1] 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26
#> [26] 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3

# 3. Season - direction defaults to "forward", season_buffer defaults to 1
lookup_timestep_sequence(bf, "prebreeding_migration")
#>  [1]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
lookup_timestep_sequence(bf, "prebreeding_migration", season_buffer = 0,
                         direction = "backward")
#>  [1] 17 16 15 14 13 12 11 10  9  8  7  6  5

# 4. start & n_steps  (start can be date or timestep)
lookup_timestep_sequence(bf, start = "2022-04-11", n_steps = 5)
#> [1] 15 16 17 18 19 20
lookup_timestep_sequence(bf, start = 10, n_steps = 5)
#> [1] 10 11 12 13 14 15

# 5.  No time arguments, equivalent to season = "all"
lookup_timestep_sequence(bf)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#> [51] 51 52  1
lookup_timestep_sequence(bf, season = "all", direction = "backward")
#>  [1]  1 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29
#> [26] 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4
#> [51]  3  2  1
```
