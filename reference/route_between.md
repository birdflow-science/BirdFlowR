# Generate BirdFlow routes between known locations

`route_between()` generates synthetic migration routes conditioned on
observed locations at specific times, using a Hidden Markov Model
forward-filtering-backward-sampling algorithm. Use `route_between()`
when you have information about where a bird or population is at two or
more points in time and want to produce routes that pass through those
locations.

## Usage

``` r
route_between(
  bf,
  n,
  x_coord = NULL,
  y_coord = NULL,
  date = NULL,
  potentials = NULL,
  ...
)
```

## Arguments

- bf:

  A BirdFlow object.

- n:

  Number of routes to sample.

- x_coord, y_coord:

  Parallel numeric vectors of observed x and y coordinates (in the
  model's CRS). Must be provided together with `date`. Converted
  internally to indicator (one-hot) potentials. Cannot be used together
  with `potentials`.

- date:

  When using `x_coord`/`y_coord`: a parallel vector of dates
  corresponding to each observation. When using `potentials`: a vector
  of dates corresponding to the columns of `potentials` (alternative to
  column names). Accepts any format recognized by
  [`lookup_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep.md):
  date strings (`"2023-03-29"`), `Date` objects, numeric timestep
  indices, or `"t1"`-style strings.

- potentials:

  An `n_active(bf)` x `n_obs` matrix of soft observation potentials.
  Each column is a non-negative vector representing the observation
  likelihood at a given timestep. Timestep association is via column
  names or the `date` argument (exactly one required). Cannot be used
  together with `x_coord`/`y_coord`.

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

A
[BirdFlowRoutes](https://birdflow-science.github.io/BirdFlowR/reference/Routes-internal.md)
object. Same format as
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md).

## See also

[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
generates routes that rely only on the location at the start of the
route.
[`predict_between()`](https://birdflow-science.github.io/BirdFlowR/reference/predict_between.md)
has the same inputs as `route_between()` but returns distributions at
the intermediate times instead of sampled routes.

## Examples

``` r
bf <- BirdFlowModels::amewoo
xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
rts <- route_between(bf, n = 5,
                     x_coord = xy$x, y_coord = xy$y,
                     date = c("2023-02-15", "2023-05-01"))
plot_routes(rts)
```
