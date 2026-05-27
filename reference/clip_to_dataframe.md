# Convert a clip polygon to a flat data frame for storage

Internal helper that converts a polygon (as `SpatVector`, `sf`, or
anything
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
accepts) into a flat data frame suitable for serializing inside a
`BirdFlow` object's metadata. The data frame has columns `id` (polygon /
object index), `part` (ring group within polygon), `x`, `y` (vertex
coordinates), and `hole` (integer ring index: `0` for the outer ring of
a part and `1..N` for each distinct hole ring within the same part). The
schema mirrors the matrix that
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
accepts when `type = "polygons"`, which is what
[`dataframe_to_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/dataframe_to_clip.md)
uses to round-trip the polygon back into spatial form.

## Usage

``` r
clip_to_dataframe(clip)
```

## Arguments

- clip:

  A polygon, in any form accepted by
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

## Value

A data frame with columns `id`, `part`, `x`, `y`, `hole`.

## Details

Storing the polygon as plain columns avoids serializing terra/sf objects
(which hold C++ pointers / environments and don't survive
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) or HDF5 round-trips
reliably).

Earlier versions of this code stored `hole` as a logical, which
collapsed multiple holes within one part into a single ring on
round-trip; the integer index keeps each hole distinct.

## See also

[`dataframe_to_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/dataframe_to_clip.md)
for the inverse.
