# Print summary of route types, their count, and number of points

This internal helper function is for use in the print methods for the
route and interval data classes.

## Usage

``` r
print_type_breakdown(x, crossline)
```

## Arguments

- x:

  A `Routes`, `BirdFlowROutes` or `BirdFlowIntervals` object.

- crossline:

  Characters to be printed after printing the summary

## Value

invisibly returns the table

## Details

It only prints the breakdown of routes by type if there is more than one
type of route in `x`. When there's only one type the output would be
redundant.
