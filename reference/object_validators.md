# Object Validators

A collection of functions to validate input data frames for `Routes`,
`BirdFlowRoutes`, and `BirdFlowIntervals`. These validators ensure that
the data frames contain all required columns, conform to expected data
types, and adhere to additional constraints specific to each object
class.

## Usage

``` r
validate_route_df(df, class = "Routes")

validate_interval_df(df)

validate_routes(routes)

validate_Routes(routes)

validate_birdflow_routes(birdflow_routes)

validate_BirdFlowRoutes(birdflow_routes)

validate_birdflow_intervals(birdflow_intervals)

validate_BirdFlowIntervals(birdflow_intervals)
```

## Arguments

- df:

  A data frame containing data for the `Routes` or `BirdFlowRoutes`
  class (`validate_route_df()`) or the `BirdFlowIntervals` class
  (`validate_interval_df()`).

- class:

  Either `"Routes"` or `"BirdFlowRoutes"`; selects which set of columns
  and checks `validate_route_df()` applies.

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

- `validate_route_df()`: Validates the input data frame for the `Routes`
  or `BirdFlowRoutes` classes, depending on `class`.

- `validate_interval_df()`: Validates the input data frame for the
  `BirdFlowIntervals` class.

## See also

- [Attribute
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/attribute_validators)

- [Column Targeting
  Functions](https://birdflow-science.github.io/BirdFlowR/reference/target_columns)

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
bfr <- route(bf, n = 3)
rts <- as_routes(bfr$data, species = bfr$species)
validate_routes(rts)
validate_birdflow_routes(bfr)

ivl <- as_birdflow_intervals(bfr)
validate_birdflow_intervals(ivl)
# }
```
