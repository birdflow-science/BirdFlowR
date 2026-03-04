# Generate BirdFlow routes

`route()` projects bird positions over time based on the probabilities
embedded in a BirdFlow model. The output is linear, stochastic routes.

## Usage

``` r
route(bf, n = 1, x_coord = NULL, y_coord = NULL, from_marginals = FALSE, ...)
```

## Arguments

- bf:

  A BirdFlow object.

- n:

  If sampling starting positions (`x_coord`, and `y_coord` are NULL).
  Generate this many samples. Otherwise the `x_coord` and `y_coord`
  positions will each be duplicated `n` times.

- x_coord, y_coord:

  Optional, if `NULL` starting points will be drawn from the species
  distribution at the initial timestep.

- from_marginals:

  Use `FALSE` (the default) to use distributions derived directly from
  eBird Status and Trends when sampling starting locations. Set to
  `TRUE` to sample from distributions derived from the fitted model
  parameters stored in the marginals. Passed to
  [`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md).

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

A BirdFlowRoutes object with columns:

- `x`, `y`:

  Coordinates of point along route.

- `date`:

  Date associated with that point.

- `timestep`:

  Timestep associated with point.

- `route`:

  Unique ID for that route or individual.

- `i`:

  Location index for the point (see
  [`i_to_xy()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)).

- `stay_id`:

  Within each route a sequential id for locations.

- `stay_len`:

  How many timesteps was the Bird at that point during the stay (minimum
  of 1).

It also has **experimental** attributes:

- `geom`, `species`, `dates`:

  The `geom`, `species`, and `dates` components of the BirdFlow object
  the routes are derived from.

- `metadata`:

  The `metadata` component of the parent BirdFlow object, with one
  additional item `route_type = "synthetic"`.

## See also

[`plot_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md),
[`animate_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_routes.md)

## Examples

``` r
bf <- BirdFlowModels::amewoo
rts <- route(bf, 10, season = "prebreeding")

if (FALSE) { # \dontrun{
plot_routes(rts)
} # }
```
