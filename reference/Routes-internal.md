# Internal (private) routes and intervals class creation functions

Internal (private) functions to create and validate `Routes`,
`BirdFlowRoutes`, and `BirdFlowIntervals` objects.

These functions ensure input data meets the required structure and
standard for use within BirdFlow models.

## Usage

``` r
new_routes(data, species, source)

new_birdflow_routes(
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

new_birdflow_intervals(data, species, metadata, geom, dates, source = NULL)
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

- **`new_routes()`**: Creates a `Routes` object from a data frame.

- **`new_birdflow_routes()`**: Creates a `BirdFlowRoutes` object,
  extending `Routes` with additional BirdFlow-specific spatial and
  temporal information.

- **`new_birdflow_intervals()`**: Creates a `BirdFlowIntervals` object,
  representing intervals between timesteps in BirdFlow data.

All objects are internally validated during creation, ensuring required
columns, valid data types, and proper formats.

## See also

- [`as_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_routes.md)
  Create a `Routes` object

- [`as_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_routes.md)
  Convert `Routes` to `BirdFlowRoutes`

- [`as_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_intervals.md)
  Extract movement between pairs of locations from `BirdFlowRoutes` for
  use with model evaluation.

- [Object
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/?object_validators)
  Private functions for validating routes and intervals.
