# Convert to a BirdFlow distribution

Methods to convert raster or point data to distributions. Each layer of
a raster will be treated as a distribution. With point data each point
will be treated as a "one hot" distribution containing mostly 0 and a
single 1 at the location corresponding to the point.

## Usage

``` r
as_distr(x, bf, ...)

# S3 method for class 'data.frame'
as_distr(x, bf, crs = NULL, ...)

# S3 method for class 'SpatRaster'
as_distr(x, bf, normalize = TRUE, zero_na = TRUE, ...)

# S3 method for class 'sf'
as_distr(x, bf, ...)
```

## Arguments

- x:

  An object to be converted either a data.frame with x and y columns
  indicating point locations, a *sf* object containing points, or a
  raster object containing values to be treated as a distribution.

- bf:

  A reference BirdFlow object.

- ...:

  Arguments used by other methods:

- crs:

  The coordinate reference system that the x and y coordinates are in.
  If NULL the function will assume coordinates are in the same CRS as
  `bf`.

- normalize:

  if `TRUE` normalize each distribution to sum to 1

- zero_na:

  if `TRUE` replace `NA` values with `0`.

## Value

An object containing distribution data to be projected with x. Either a
vector with
[n_active(bf)](https://birdflow-science.github.io/BirdFlowR/reference/n_active())
values or a matrix with that many rows and a column for each
distribution.

## Details

If `x` is a data.frame it should have columns `x` and `y` containing the
coordinates of points. `crs` should indicate the coordinate reference
system the points are in - only necessary if it differs from `crs(bf)`.

If `x` is either an *sf* object or a data frame it should represent
points. Each point will be converted into a separate distribution with
the cell corresponding to the point having a value of 1 with the
remaining values set to 0.

If `x` is a
[`terra::SpatRaster()`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
it will be projected to align with the cells in `bf` with
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html)
using `method = "average"` and then cropped and/or extended to match the
extent of `bf`. Warnings will be throw if some of the value in `x` is
lost due to cropping or due to masking out the inactive cells.
