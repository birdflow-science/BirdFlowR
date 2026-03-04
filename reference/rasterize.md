# Convert a BirdFlow distribution into a raster

[`rast()`](https://rspatial.github.io/terra/reference/rast.html)
converts a BirdFlow object directly to a
[SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
`rasterize_distr()` converts a
[distribution](https://birdflow-science.github.io/BirdFlowR/reference/as_distr)
into a
[SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
numeric matrix or array, or a raster data frame.

## Usage

``` r
rasterize_distr(distr, bf, format = "SpatRaster")

# S4 method for class 'BirdFlow'
rast(x, which = "all")
```

## Arguments

- distr:

  A distribution in its vector form or a matrix in which each column
  represents a different distribution.

- bf:

  A BirdFlow object.

- format:

  One of `'SpatRaster'` for a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object, `'numeric'` for a matrix or array, or`'dataframe'` for raster
  data suitable for plotting with
  [`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)

- x:

  A BirdFlow object.

- which:

  Indicates which timesteps to return. Can be one or more integers
  indicating timesteps; character dates in the format year-month-day
  e.g. `"2019-02-25"`; [`Date`](https://rdrr.io/r/base/Dates.html)
  objects; or `"all"` which will return distributions for all timesteps.

## Value

For `rasterize_distr()` the return type depends on the `format`
argument:

- `"SpatRaster"` (the default) returns a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object.

- `"numeric"` returns a matrix (one distribution) or array (multiple
  distributions). In either case the first two dimensions will be y
  (rows), and x (columns).

- `"dataframe"` will return the raster information in a data frame with
  a row for every value (long format) with columns:

  - `x`, `y` the x and y coordinates of the cell center.

  - `i` the location index (in `bf`) of the cell.

  - `label` The label associated with the distribution, taken from
    column names of `distr` - typically it indicates time. It is an
    ordered factor with the level order matching the order of the
    distribution in `distr`. The ordered factor is helpful when
    animating.

  - `value` The cell value, typically density

  - `order` The column index of the distribution in `distr` or if only
    one distribution `1`. The object is suitable for plotting with
    \[[ggplot2::geom_raster](https://ggplot2.tidyverse.org/reference/geom_tile.html)\].

[`rast()`](https://rspatial.github.io/terra/reference/rast.html) returns
a
[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
