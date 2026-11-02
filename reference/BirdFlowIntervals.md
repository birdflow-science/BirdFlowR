# Deprecated function to make a BirdFlowIntervals object from a data frame

This function is deprecated. Please use
[`as_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_intervals.md)
instead (its `data.frame` method).

## Usage

``` r
BirdFlowIntervals(data, species, metadata, geom, dates, source = NULL)
```

## Arguments

- species:

  A list with species information. Only used by the `data.frame` method.

- metadata:

  A list with additional metadata. Only used by the `data.frame` method.

- geom:

  A list describing spatial geometry. Only used by the `data.frame`
  method.

- dates:

  A data frame with date-related information. Only used by the
  `data.frame` method.

- source:

  A character string indicating the source of the data. Only used by the
  `data.frame` method.

## Value

A `BirdFlowIntervals` object.

## See also

[`as_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_intervals.md)
should be used instead of this function.
