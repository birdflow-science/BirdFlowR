# Format raster data from a BirdFlow model for output

Internal helper shared by
[`get_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_mask.md)
and
[`get_ebird_coverage()`](https://birdflow-science.github.io/BirdFlowR/reference/get_ebird_coverage.md).
Handles format-string normalization, alias resolution, and the three
output formats.

## Usage

``` r
format_raster_data(data, bf, format, value_name)
```

## Arguments

- data:

  A 2-D logical matrix (e.g. the model mask) or 3-D logical array (e.g.
  `ebird_coverage`) aligned to the BirdFlow model grid. For 3-D arrays
  the third dimension is time; its `dimnames$time` element is used to
  name raster layers and the `timestep` column.

- bf:

  A BirdFlow model (supplies grid geometry).

- format:

  A character string naming the output format. Recognized values
  (case-insensitive, with aliases):

  - `"SpatRaster"` / `"spatrast"` / `"terra"` —
    [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)

  - `"numeric"` / `"array"` / `"matrix"` — raw data returned as-is

  - `"dataframe"` / `"data.frame"` / `"raster.data.frame"` — long
    [data.frame](https://rdrr.io/r/base/data.frame.html)

- value_name:

  Column name for the data values in `"dataframe"` output and layer name
  for single-layer `"SpatRaster"` output.

## Value

Depends on `format`:

- `"spatraster"` — a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html);
  one layer per time slice for 3-D input, a single named layer for 2-D
  input.

- `"numeric"` — `data` returned as-is.

- `"dataframe"` — a [data.frame](https://rdrr.io/r/base/data.frame.html)
  with columns `row`, `col`, `x`, `y`, `i` (NA outside the mask), and
  `<value_name>`. For 3-D input the frame is in long form (one row per
  cell × timestep) with an additional `timestep` column preceding
  `<value_name>`.

## See also

[`get_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_mask.md),
[`get_ebird_coverage()`](https://birdflow-science.github.io/BirdFlowR/reference/get_ebird_coverage.md)
