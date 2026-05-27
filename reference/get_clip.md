# Retrieve the clip polygon used during preprocessing

Reconstructs the clip polygon that was supplied to
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
from the metadata stored on the `BirdFlow` object. Returns an
[sf](https://r-spatial.github.io/sf/reference/sf.html) polygon in the
model's CRS (`crs(x)`).

## Usage

``` r
get_clip(x)
```

## Arguments

- x:

  A `BirdFlow` model.

## Value

An `sf` object containing the clip polygon, or `NULL` if the model is
unclipped or the clip metadata is unknown (older models preprocessed
before this metadata was captured).

## Details

The polygon is stored as a flat data frame (see
[`clip_to_dataframe()`](https://birdflow-science.github.io/BirdFlowR/reference/clip_to_dataframe.md))
so it can be serialized as part of the model. `get_clip()` delegates the
data-frame-to-polygon conversion to
[`dataframe_to_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/dataframe_to_clip.md)
and wraps the result in an `sf` object; pass the result through
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
if you'd prefer a
[terra::SpatVector](https://rspatial.github.io/terra/reference/SpatVector-class.html).

## See also

[`is_clipped()`](https://birdflow-science.github.io/BirdFlowR/reference/is_clipped.md),
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md),
[`dataframe_to_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/dataframe_to_clip.md).

## Examples

``` r
  library(BirdFlowModels)
  get_clip(amewoo) # NULL — fixture predates clip metadata
#> NULL
```
