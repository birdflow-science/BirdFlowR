# Calculate stats from a spatRaster with abundances for each timestep

Calculate the total number of parameters and the occupied area for each
timestep from an abundance raster.

## Usage

``` r
calc_abundance_stats(x, circular = TRUE)
```

## Arguments

- x:

  a multilayer spatRaster with relative abundances for each timestep

- circular:

  if TRUE (the default) use the first timestep abundance as the ending
  abundance.

## Value

A list with

- n_params:

  The number of parameters there would be in the model if fit on `x`

- count:

  a vector of the number of cells occupied at each timestep

- area:

  a vector of area (sq m) that is in included cells for each timestep

- res:

  the resolution of the raster in km

## Details

This is a helper to
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
