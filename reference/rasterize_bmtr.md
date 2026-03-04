# Rasterize BMTR Data

Converts a BMTR dataframe into a multi-layer raster, with each unique
transition as a separate layer.

## Usage

``` r
rasterize_bmtr(bmtr, bf)
```

## Arguments

- bmtr:

  A dataframe returned by `calc_bmtr`, containing columns `x`, `y`,
  `transition`, `bmtr`, and `date`.

- bf:

  A BirdFlow model. For example:
  [`BirdFlowModels::amewoo`](https://rdrr.io/pkg/BirdFlowModels/man/BirdFlowModels.html)

## Value

A `SpatRaster` object (from `terra`) with layers corresponding to unique
transitions. Missing values in `bmtr` are represented as `NA`.

## Details

The `x` and `y` coordinates are mapped to raster columns and rows.

## Examples

``` r
if (FALSE) { # \dontrun{
raster <- rasterize_bmtr(bmtr, bf)
plot(raster)
} # }
```
