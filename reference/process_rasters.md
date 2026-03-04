# process_rasters

Internal function to process rasters from eBird Status and Trends for
use with a BirdFlow model. Called only from
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
but sufficiently complicated to justify being a separate function.

## Usage

``` r
process_rasters(
  res,
  crs,
  download_species,
  sp_path,
  clip,
  project_method,
  download_patterns,
  trim_quantile = NULL
)
```

## Arguments

- res:

  Output resolution in kilometers

- crs:

  Coordinate reference system (CRS) to use. Defaults to the custom
  projection eBird has assigned to this species - see
  [`ebirdst::load_fac_map_parameters()`](https://ebird.github.io/ebirdst/reference/load_fac_map_parameters.html)).
  It will be interpreted by
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html)
  to generate a well known text representation of the CRS.

- download_species:

  The species code used when downloading eBird S&T data, might be
  "example_data" but otherwise a standard species code.

- sp_path:

  The path used when downloading the species data - passed to ebirdst
  functions.

- clip:

  polygon indicating the area to process or NULL to process entire
  species range.

- project_method:

  Method to use when reprojecting. Set locally by code within
  [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)

- trim_quantile:

  With the default of `NULL` there is no outlier trimming, otherwise a
  single value between 0 and 1 to indicate the quantile to truncate at
  or a series of 52 such values corresponding with each week. Trimming
  outliers is always done week by week with the values above the
  `trim_quantile` quantile set to the value of that quantile. Reasonable
  non `NULL` values will be close to 1 e.g. 0.99, 0.995, 0.999. Set
  `trim_quantile` to eliminate high outliers that are believed to be
  model artifacts. See [Issue
  \#189](https://github.com/birdflow-science/BirdFlowR/issues/189) for
  detailed justification.

## Value

A list with

- distr, uci, lci:

  The are the species distribution, and upper and lower confidence
  intervals on that distribution in their flattened form. Each timestep
  is stored in a column with values for unmasked cells only.

- m:

  The mask stored as a logical matrix with TRUE representing active
  cells in the model

- mask:

  The mask stored as a terra::SpatRaster
