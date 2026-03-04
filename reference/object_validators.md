# Object Validators

A collection of functions to validate input data frames for `Routes`,
`BirdFlowRoutes`, and `BirdFlowIntervals`. These validators ensure that
the data frames contain all required columns, conform to expected data
types, and adhere to additional constraints specific to each object
class.

## Usage

``` r
validate_Routes_route_df(route_df)

validate_BirdFlowRoutes_birdflow_route_df(birdflow_route_df)

validate_BirdFlowIntervals_birdflow_intervals(birdflow_interval_df)

validate_Routes(routes)

validate_BirdFlowRoutes(birdflow_routes)

validate_BirdFlowIntervals(birdflow_intervals)
```

## Arguments

- route_df:

  A data frame containing data for the `Routes` class. It must include
  columns like `route_id`, `date`, `lon`, `lat`, and `route_type`.

- birdflow_route_df:

  A data frame containing data for the `BirdFlowRoutes` class. It must
  include additional columns such as `x`, `y`, `i`, and `timestep`.

- birdflow_interval_df:

  A data frame containing data for the `BirdFlowIntervals` class. It
  must include columns such as `lon1`, `lon2`, `y1`, `y2`, `i1`, `i2`,
  `timestep1`, `timestep2`.

- routes:

  A `Routes` object.

- birdflow_routes:

  A `BirdFlowRoutes` object.

- birdflow_intervals:

  A `BirdFlowIntervals` object.

## Value

These functions return nothing if validation succeeds. If validation
fails, an error message is raised detailing the issue.

## Details

These functions perform comprehensive checks to ensure data integrity.
They verify that:

- All required columns are present.

- Data types match expected formats (e.g., numeric, character, Date).

- No missing or invalid values are present in critical fields.

- Additional constraints specific to the class, such as unique timesteps
  for routes, are satisfied.

### Functions Included:

- `validate_Routes_route_df()`: Validates the input data frame for the
  `Routes` class.

- `validate_BirdFlowRoutes_birdflow_route_df()`: Validates the input
  data frame for the `BirdFlowRoutes` class.

- `validate_BirdFlowIntervals_birdflow_intervals()`: Validates the input
  data frame for the `BirdFlowIntervals` class.

## See also

- [Attribute
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/?attribute_validators)

- [Column Targeting
  Functions](https://birdflow-science.github.io/BirdFlowR/reference/?target_columns)
