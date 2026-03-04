# Get Target Columns for Data Frames

A collection of internal utility functions to retrieve the required
column names for `Routes`, `BirdFlowRoutes`, and `BirdFlowIntervals`
objects. These functions ensure consistency in data frame structures
across different processing steps.

## Usage

``` r
get_target_columns_Routes(type = "input")

get_target_columns_BirdFlowRoutes(type = "input")

get_target_columns_BirdFlowIntervals(type = "input")
```

## Arguments

- type:

  A character string specifying the context for the columns. Either
  `'input'` (default) for required input columns or `'output'` for
  columns expected after processing.

## Value

A character vector containing the expected column names.

## Details

These functions return the expected column names for `Routes`,
`BirdFlowRoutes` and `BirdFlowIntervals` data classes. Columns may vary
depending on whether the context is for input or output processing:

- **Input Columns**: Columns required for validation or initial data
  ingestion.

- **Output Columns**: Columns expected after processing or
  transformation.

### Functions Included:

- `get_target_columns_Routes()`: Returns column names for `Routes`
  objects.

- `get_target_columns_BirdFlowRoutes()`: Returns column names for
  `BirdFlowRoutes` objects.

- `get_target_columns_BirdFlowIntervals()`: Returns column names for
  `BirdFlowIntervals` objects.

## See also

- [Object
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/?object_validators)

- [Attribute
  Validators](https://birdflow-science.github.io/BirdFlowR/reference/?attribute_validators)
