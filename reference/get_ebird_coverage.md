# Get eBird model coverage from a BirdFlow model

`get_ebird_coverage()` retrieves the per-timestep eBird model coverage
stored in a BirdFlow model. Each cell–timestep combination records
whether that cell fell within eBird's modeled extent at that timestep.

## Usage

``` r
get_ebird_coverage(bf, format = "SpatRaster")
```

## Arguments

- bf:

  A BirdFlow model.

- format:

  One of `"SpatRaster"` (default), `"array"`, or `"dataframe"`. See the
  return section for details. Format strings are matched
  case-insensitively; common aliases are accepted (e.g. `"terra"`,
  `"numeric"`, `"matrix"`, `"data.frame"`).

## Value

`NA` (with a warning) if the model lacks coverage metadata. Otherwise
the return type depends on `format`:

- `"SpatRaster"` returns a multi-layer
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with one logical layer per timestep. Layer names match the timestep
  labels (e.g. `"January 4"` with the default `time_format`).

- `"array"` returns the 3-D logical array `[row, col, time]`, with
  timestep labels applied to the `"time"` dimension.

- `"dataframe"` returns a long
  [data.frame](https://rdrr.io/r/base/data.frame.html) suitable for
  plotting with
  [`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
  with one row per cell × timestep and columns:

  - `row`, `col` — raster row and column indices.

  - `x`, `y` — cell-center coordinates in the model CRS.

  - `i` — location index in `bf`; `NA` for cells outside the mask.

  - `timestep` — canonical timestep label (e.g. `"t1"`).

  - `label` — `time_format`-formatted timestep label (e.g.
    `"January 4"`).

  - `coverage` — logical coverage value.

## Details

Coverage metadata is captured by
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
and is absent from models preprocessed before `BirdFlowR 0.1.0.9081`.
For such models `get_ebird_coverage()` returns `NA` with a warning.

Timestep labels in the `"time"` dimension of `"array"` output and layer
names of `"SpatRaster"` output follow
[birdflow_options("time_format")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md),
the same as
[`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md)
and
[`get_dynamic_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_dynamic_mask.md).
In `"dataframe"` output the canonical labels (`"t1"`, `"t2"`, ...) are
always in the `timestep` column, with the `time_format`-formatted
version in `label`. The internal, canonical labels are always used in
`metadata$ebird_coverage` itself; only the labels returned here change.

## See also

[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
which captures this metadata,
[`get_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_mask.md)
for the static model mask,
[`birdflow_options()`](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md)
for `time_format`.

## Examples

``` r
if (FALSE) { # \dontrun{
bf <- preprocess_species("amewoo")
cov <- get_ebird_coverage(bf)               # SpatRaster, one layer per week
arr <- get_ebird_coverage(bf, "array")      # raw 3-D logical array
df  <- get_ebird_coverage(bf, "dataframe")  # long data frame for ggplot2
} # }
```
