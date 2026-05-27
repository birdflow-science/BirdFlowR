# Test whether a BirdFlow model was clipped during preprocessing

`is_clipped()` reports whether a clip polygon was supplied to
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
when the model was built.

## Usage

``` r
is_clipped(x)
```

## Arguments

- x:

  A `BirdFlow` model.

## Value

A logical scalar: `TRUE` if a clip was applied, `FALSE` if the model was
preprocessed without clipping, or `NA` if the metadata is missing
(legacy models).

## Details

Returns `NA` for models built before clip metadata was recorded — there
is no way to recover this information from older HDF5 / RDS files. Use
[`get_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/get_clip.md)
to retrieve the polygon itself.

## See also

[`get_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/get_clip.md),
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md).

## Examples

``` r
  library(BirdFlowModels)
  is_clipped(amewoo) # NA — fixture predates clip metadata
#> [1] NA
```
