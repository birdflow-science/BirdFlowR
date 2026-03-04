# Attribute Validators

A collection of internal utility functions to validate individual
attributes within `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals`,
and related objects. These functions ensure the correctness,
consistency, and validity of attributes used in BirdFlow data
structures.

## Usage

``` r
validate_Routes_route_id(route_id)

validate_Routes_date(date_vector)

validate_Routes_lon(lon_vector)

validate_Routes_lat(lat_vector)

validate_Routes_route_type(route_type_vector)

validate_BirdFlowRoutes_route_type(route_type_vector)

validate_BirdFlowRoutes_x(x_vector)

validate_BirdFlowRoutes_y(y_vector)

validate_BirdFlowRoutes_i(i_vector)

validate_BirdFlowRoutes_timestep(timestep_vector)

validate_BirdFlowRoutes_date(route_id_vector, date_vector)

validate_BirdFlowRoutes_stay_id(stay_id_vector)

validate_BirdFlowRoutes_stay_len(stay_len_vector)

validate_BirdFlowRoutes_species(species)

validate_BirdFlowRoutes_metadata(metadata)

validate_BirdFlowRoutes_dates(dates)

validate_BirdFlowIntervals_interval_id(interval_id)

validate_BirdFlowIntervals_metadata(metadata)
```

## Arguments

- route_id:

  A vector of route IDs (character or numeric). Must not contain missing
  values.

- date_vector:

  A vector of dates (`Date`, `POSIXct`, or `POSIXlt`). Must not contain
  missing values; Must not have duplicates within a `route_id` (for
  `BirdFlowRoutes`; but not for `Routes`).

- lon_vector, lat_vector:

  Numeric vectors for longitude and latitude. Must not contain missing
  values and must be within valid ranges.

- route_type_vector:

  A character vector of route types. Must only contain valid types.

- x_vector, y_vector:

  Numeric vectors for spatial coordinates. Must not contain missing
  values.

- i_vector:

  An integer vector for spatial indices. Must not contain missing
  values.

- timestep_vector:

  An integer vector for timesteps, paired with `route_id_vector`.

- stay_id_vector:

  A numeric or character vector for stay IDs. Must not contain missing
  values.

- stay_len_vector:

  An integer vector for stay lengths. Must not contain missing values.

- species:

  A list with species information. Must include `species_code`,
  `scientific_name`, and `common_name`.

- metadata:

  A list with additional metadata.

- dates:

  A data frame with date-related information. Must include `interval`,
  `date`, `midpoint`, `start`, `end`, `doy`, and `week`.

## Value

Each function returns `TRUE` if validation succeeds. If validation
fails, an error is raised with details about the issue.

## Details

These functions validate specific attributes or columns in input data
frames. They check:

- **General Attributes**:

  - `route_id`: Ensures IDs are valid and non-missing.

  - `date`: Ensures date formats and non-missing values.

  - `lon` and `lat`: Ensure longitude and latitude values are within
    valid ranges.

  - `route_type`: Ensures only valid route types are present.

- **Spatial Attributes** (for `BirdFlowRoutes`):

  - `x` and `y`: Ensure numeric spatial coordinates.

  - `i`: Ensures valid spatial indices as integers.

  - `timestep`: Ensures unique timesteps per `route_id`.

  - `stay_id` and `stay_len`: Ensure valid stay identifiers and lengths.

- **Additional Attributes**:

  - `species`: Ensures species data contains required components.

  - `geom`: Ensures geometry data includes all required fields.

  - `dates`: Validates date-related data frames, checking required
    columns.

These functions are intended for internal use and are not exported for
user-facing functionality.

### Functions Included:

- General Validators:

  - `validate_Routes_route_id()`

  - `validate_Routes_date()`

  - `validate_Routes_lon()`, `validate_Routes_lat()`

  - `validate_Routes_route_type()`

- `BirdFlowRoutes` Validators:

  - `validate_BirdFlowRoutes_x()`, `validate_BirdFlowRoutes_y()`

  - `validate_BirdFlowRoutes_i()`, `validate_BirdFlowRoutes_timestep()`,

  - `validate_BirdFlowRoutes_date()`,

  - `validate_BirdFlowRoutes_stay_id()`,
    `validate_BirdFlowRoutes_stay_len()`

  - `validate_BirdFlowRoutes_species()`

- `BirdFlowIntervals` Validators:

  - `validate_BirdFlowIntervals_interval_id()`

- Dates and Metadata:

  - `validate_BirdFlowRoutes_dates()`

  - `validate_BirdFlowRoutes_metadata()`

## See also

- [Object
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/?object_validators)

- [Column Targeting
  Functions](https://birdflow-science.github.io/BirdFlowR/reference/?target_columns)
