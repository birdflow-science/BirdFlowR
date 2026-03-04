# Convert a collapsed distribution into a expanded, raster equivalent

This function converts a collapsed distribution in which the first (and
possibly only) dimension represents all the locations in the model into
an expanded, raster state in which the first two dimensions represent
the locations of the cell in physical space (row, col) and are thus
suitable for plotting or conversion into spatial objects.

## Usage

``` r
expand_distr(distr, bf)
```

## Arguments

- distr:

  Either a vector representing a single distribution with one value per
  location in the model or a matrix in which each column is such a
  vector. Higher dimensions are allowed (but unlikely); in all cases the
  first dimension is for locations in the model.

- bf:

  A BirdFlow model

## Value

An expanded version of `distr` with one additional dimension, in which
the first two dimensions are rows and columns in space (a raster) and
replace the first dimension in the input.

## Details

In its collapsed form a single distribution is stored as a vector. Each
value can be interpreted as the relative abundance or probability of
finding a bird at the corresponding location. A special case is when the
vector has mostly 0's and a single 1 in which case it represents a
single position of a bird or group of birds, a very concentrated
distribution.

Additional dimensions can represent multiple timesteps and/or multiple
individual birds, model runs etc.

The location information in the collapsed distribution is not easily
accessible as position in the vector is ordered based on row-major
ordered unmasked cells in the extent, and R uses column-major order.

As of May 2023 this is now an internal function replaced by
[rasterize_distr(format =
"numeric")](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md).
