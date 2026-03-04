# Internal (private) routes and intervals class creation functions

Internal (private) functions to create and validate `Routes`,
`BirdFlowRoutes`, and `BirdFlowIntervals` objects.

These functions ensure input data meets the required structure and
standard for use within BirdFlow models.

## Usage

``` r
new_Routes(data, species, source)

BirdFlowRoutes(
  data,
  species,
  metadata,
  geom,
  dates,
  source = NULL,
  sort_id_and_dates = TRUE,
  reset_index = FALSE,
  stay_calculate_col = "date"
)

new_BirdFlowRoutes(
  data,
  species,
  metadata,
  geom,
  dates,
  source,
  stay_calculate_col = "date",
  sort_id_and_dates = FALSE
)

BirdFlowIntervals(data, species, metadata, geom, dates, source = NULL)

new_BirdFlowIntervals(data, species, metadata, geom, dates, source)
```

## Arguments

- data:

  A data frame containing route/interval data for `Routes`,
  `BirdFlowRoutes` or `BirdFlowIntervals`.

- species:

  Either a single character that will be passed to
  [`ebirdst::get_species()`](https://ebird.github.io/ebirdst/reference/get_species.html)
  to lookup species information or a list with species metadata which
  must include `common_name` and can optionally also include
  `scientific_name` and `species_code` or any other standard BirdFlow
  species metadata. See
  [`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
  for a description of the full list.

- source:

  A character string indicating the source of the data.

- metadata:

  A list with additional metadata.

- geom:

  A list describing spatial geometry, such as `nrow`, `ncol`, `crs`, and
  `mask`.

- dates:

  A data frame with date-related information, including `date`, `start`,
  `end`, and `timestep`.

- sort_id_and_dates:

  Logical. Should the data be sorted by `route_id` and `dates`?

- reset_index:

  Logical. Should the index of the data frame be reset after sorting?

- stay_calculate_col:

  The column name for calculating the `stay_id` and `stay_len` in
  `BirdFlowRoutes` object. Defaults to `date`.

## Value

Each function returns an S3 object of the corresponding class (`Routes`,
`BirdFlowRoutes`, or `BirdFlowIntervals`).

## Details

- **[`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md)**:
  Creates a `Routes` object from a data frame.

- **`BirdFlowRoutes()`**: Creates a `BirdFlowRoutes` object, extending
  `Routes` with additional BirdFlow-specific spatial and temporal
  information.

- **`BirdFlowIntervals()`**: Creates a `BirdFlowIntervals` object,
  representing intervals between timesteps in BirdFlow data.

All objects are internally validated during creation, ensuring required
columns, valid data types, and proper formats. Non-exported `new_*`
functions handle the final assembly of the object after validation.

## See also

- [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md)
  Create a `Routes` object

- [`as_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md)
  Convert `Routes` to `BirdFlowRoutes`

- [`as_BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowIntervals.md)
  Extract movement between pairs of locations from BirdFlowRoutes for
  use with model evaluation.

- [Object
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/?object_validators)
  Private functions for validating routes and intervals.

## Examples

``` r
# Examples here use private functions so can't be run except after
# devtools::load_all() during package development.
if (FALSE) { # \dontrun{

# Create a Routes object
route_df <- data.frame(
  route_id = c("001", "001", "001", "001", "001",
  "003", "003", "003", "004"),
  date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21",
  "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
  lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
  -89.6298, -85.6298, -95.3698),
  lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781,
  42.8781, 40.8781, 29.7604),
  route_type = c("tracking", "tracking", "tracking", "tracking",
  "tracking", "motus", "motus", "motus", "motus")
)
species <- list(
  species_code = "amewoo",
  scientific_name = "Scolopax minor",
  common_name = "American Woodcock"
)
sources <- "Unknown sources"
routes_obj <- Routes(route_df, species = species, source = sources)

# Create a BirdFlowRoutes object
## 1. convert from `Routes`
bf <- BirdFlowModels::amewoo
birdflow_route_df <- routes_obj |> as_BirdFlowRoutes(bf = bf)
# the species, metadata, and sources will be inherited from the bf object.

## 2. Directly from dataframe
birdflow_route_df <- data.frame(
  route_id = c("001", "001", "001", "001", "001", "003", "003",
  "003", "004"),
  date = as.Date(c(
    "2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21", "2025-02-10",
    "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01"
  )),
  lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
  -89.6298, -85.6298, -95.3698),
  lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781, 42.8781,
  40.8781, 29.7604),
  x = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
  y = c(1000, 2000, 1000, 2000, 1000, 2000, 1000, 2000, 1000),
  i = as.integer(c(1, 2, 1, 2, 1, 2, 1, 2, 1)),
  timestep = as.integer(c(1, 2, 3, 4, 5, 1, 2, 3, 1)),
  route_type = c(
    "tracking", "tracking", "tracking", "tracking",
    "tracking", "motus", "motus", "motus", "motus"
  )
)
geom <- list(
  nrow = 100, ncol = 200, res = 1, ext = NULL, crs = NULL,
  mask = NULL, dynamic_mask = NULL
)
dates <- data.frame(
  timestep = 1:2,
  date = as.Date(c("2022-01-04", "2022-01-11")),
  label = c("January 4", "January 11"),
  julian = c(4, 11),
  week = c(1, 2)
)
birdflowroutes_object <- BirdFlowRoutes(
  birdflow_route_df,
  species = species,
  metadata = metadata,
  geom = geom,
  dates = dates,
  source = "example_source"
)
# Create a BirdFlowIntervals object
## 1. convert from `BirdFlowRoutes`
birdflow_intervals_obj <- birdflowroutes_object |> as_BirdFlowIntervals()

## 2. Directly from dataframe
birdflow_intervals <- data.frame(
  interval_id = 1:3,
  route_id = c("route1", "route1", "route2"),
  lon1 = c(-90, -89, -88),
  lon2 = c(-89, -88, -87),
  lat1 = c(40, 41, 42),
  lat2 = c(41, 42, 43),
  x1 = c(1000, 1100, 1200),
  x2 = c(1100, 1200, 1300),
  y1 = c(500, 600, 700),
  y2 = c(600, 700, 800),
  i1 = as.integer(c(1, 2, 3)),
  i2 = as.integer(c(2, 3, 4)),
  date1 = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
  date2 = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")),
  timestep1 = as.integer(c(1, 2, 3)),
  timestep2 = as.integer(c(2, 3, 4)),
  route_type = c("tracking", "tracking", "banding")
)
birdflow_intervals_obj <- BirdFlowIntervals(
  birdflow_intervals,
  species = species,
  metadata = metadata,
  geom = geom,
  dates = dates,
  source = "example_source"
)
} # }
```
