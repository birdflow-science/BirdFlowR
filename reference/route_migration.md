# Deprecated function to generate migration routes from a BirdFlow model

This function is now deprecated and will eventually be deleted. Please
transition to using
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
which can now both generate starting locations by sampling the
distributions in `bf` and use a season name to specify the time period
to route over. The only adjustment that needs to be made is to use the
`season` argument to
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
in place of the `migration` argument to `route_migration()`.

## Usage

``` r
route_migration(bf, n, migration = "prebreeding", season_buffer = 1)
```

## Arguments

- bf:

  `BirdFlow` model

- n:

  the number of routes to generate

- migration:

  "prebreeding", "pre", or "spring" for the prebreeding migration; or
  "postbreeding", "post", or "fall" for the postbreeding migration.

- season_buffer:

  a buffer in timesteps (likely weeks) to add to the beginning and end
  of the season. The default of 1 means we start a week before the
  metadata suggests the season starts and continue one week past the
  end.

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

- [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
  should be used instead of this function.
