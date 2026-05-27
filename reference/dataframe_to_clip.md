# Reconstruct a clip polygon from its data frame representation

Inverse of
[`clip_to_dataframe()`](https://birdflow-science.github.io/BirdFlowR/reference/clip_to_dataframe.md).
Takes the flat data frame stored in a `BirdFlow` object's
`metadata$clip$polygon` slot and rebuilds an
[sf](https://r-spatial.github.io/sf/reference/sfc.html) polygon.
Factored out of
[`get_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/get_clip.md)
so the round-trip (polygon -\> data frame -\> polygon) can be exercised
in tests without constructing a full `BirdFlow` object.

## Usage

``` r
dataframe_to_clip(df, crs = NA)
```

## Arguments

- df:

  A data frame as produced by
  [`clip_to_dataframe()`](https://birdflow-science.github.io/BirdFlowR/reference/clip_to_dataframe.md)
  with columns `id`, `part`, `x`, `y`, and `hole`. The `hole` column is
  the integer ring index (`0` for outer rings, `1..N` for each distinct
  hole within a part); legacy logical values are coerced via
  [`as.integer()`](https://rdrr.io/r/base/integer.html).

- crs:

  Coordinate reference system. Accepts anything
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
  accepts (a WKT string, an `"EPSG:..."` code, an
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  object, or `NA` for no CRS).

## Value

An [sfc](https://r-spatial.github.io/sf/reference/sfc.html) polygon
(geometry only, no attributes).

## See also

[`clip_to_dataframe()`](https://birdflow-science.github.io/BirdFlowR/reference/clip_to_dataframe.md),
[`get_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/get_clip.md).
