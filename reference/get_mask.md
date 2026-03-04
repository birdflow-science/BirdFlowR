# Extract mask from BirdFlow model

`get_mask()` extracts the static mask from a BirdFlow model. The static
mask is a logical raster indicating which cells are included in the
model (at any timestep). These are also the cells (in row major order)
that correspond with distribution values, and location indices.

## Usage

``` r
get_mask(bf, format = "SpatRaster")
```

## Arguments

- bf:

  A BirdFlow model

- format:

  One of `'SpatRaster'` for a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object, `'numeric'` for a matrix or array, or`'dataframe'` for raster
  data suitable for plotting with
  [`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)

## Value

The return type of `get_mask()`depends on the `format` argument:

- `"SpatRaster"` (the default) returns a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object.

- `"numeric"` returns the mask as a matrix.

- `"dataframe"` will return a data frame suitable for plotting with
  [ggplot2::geom_raster](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  with columns:

  - `row`, `col` the row and column indices of each cell.

  - `x`, `y` the x and y coordinates of the cell center.

  - `i` the location index (in `bf`) of the cell.

  - `mask` `TRUE` for cells included in the *model*, `FALSE` for
    excluded cells.

## Examples

``` r
bf <- BirdFlowModels::amewoo
m <- get_mask(bf)

if (FALSE) { # \dontrun{
library(terra)
plot(m)
} # }
```
