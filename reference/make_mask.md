# Internal function to determine the extent of data in eBird rasters.

This function returns a logical single layer SpatRaster which is `TRUE`
if `x` has non-zero values in any layer. `NA` does not count as
non-zero. The result is cropped to the extent of the data in `x`. It's
used in BirdFlowR to preprocess data.

## Usage

``` r
make_mask(x, count = FALSE, assume_no_na = FALSE)
```

## Arguments

- x:

  A SpatRaster, typically it's multilayered and contains the
  distribution of a species over time.

- count:

  If TRUE the returned value will be the count of non-zero cells at the
  raster location (across timesteps). Otherwise a logical is returned,
  with TRUE to indicate any non-zero value.

- assume_no_na:

  Set to TRUE if there aren't any NA values in the raster for a slight
  efficiency improvement.

## Value

A single layer SpatRaster with the same CRS, alignment, and resolution
as `x`; cropped to the extent of the data in `x`. With `count = FALSE`
the result is a logical that is `TRUE` if there are non-zero cells in
any layer in `x` and `FALSE` otherwise. With `count = TRUE` the result
is an integer with a count of the number of timesteps in which the cell
appears (0 to 52 given weekly steps).

## Details

`make_mask` is a private function.
