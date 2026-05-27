# Get eBird model coverage from a BirdFlow model

`get_ebird_coverage()` retrieves the per-timestep eBird model coverage
stored in a BirdFlow model. Each cell–timestep combination records
whether that cell fell within eBird's modeled extent at that timestep;
eBird 2023 onward uses `NA` for cells with insufficient data (distinct
from cells with zero abundance).

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
  labels (e.g. `"t1"`, `"t2"`, ...).

- `"array"` returns the raw 3-D logical array `[row, col, time]` as
  stored in `metadata$ebird_coverage`.

- `"dataframe"` returns a long
  [data.frame](https://rdrr.io/r/base/data.frame.html) suitable for
  plotting with
  [`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
  with one row per cell × timestep and columns:

  - `row`, `col` — raster row and column indices.

  - `x`, `y` — cell-center coordinates in the model CRS.

  - `i` — location index in `bf`; `NA` for cells outside the mask.

  - `timestep` — timestep label (e.g. `"t1"`).

  - `coverage` — logical coverage value.

## Details

Coverage metadata is captured by
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
and is absent from models preprocessed before `BirdFlowR 0.1.0.9081`.
For such models `get_ebird_coverage()` returns `NA` with a warning.

## See also

[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
which captures this metadata,
[`get_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_mask.md)
for the static model mask.

## Examples

``` r
if (FALSE) { # \dontrun{
bf <- preprocess_species("amewoo")
cov <- get_ebird_coverage(bf)               # SpatRaster, one layer per week
arr <- get_ebird_coverage(bf, "array")      # raw 3-D logical array
df  <- get_ebird_coverage(bf, "dataframe")  # long data frame for ggplot2
} # }
```
