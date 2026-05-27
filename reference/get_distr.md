# Extract distributions from BirdFlow models

`get_distr` will return one or more distributions in their flattened
form. A single distribution will be returned as a vector; if multiple
they will be columns in a matrix.

## Usage

``` r
get_distr(
  x,
  which = "all",
  type = c("normalized", "marginal", "raw"),
  from_marginals
)
```

## Arguments

- x:

  A BirdFlow model

- which:

  Indicates which timesteps to return. Can be one or more integers
  indicating timesteps; character dates in the format year-month-day
  e.g. `"2019-02-25"`; [`Date`](https://rdrr.io/r/base/Dates.html)
  objects; or `"all"` which will return distributions for all timesteps.

- type:

  One of `"normalized"` (the default), `"marginal"`, or `"raw"`. See
  "Distribution types" for details.

- from_marginals:

  Deprecated. Use `type = "marginal"` instead. When supplied,
  `from_marginals = TRUE` is translated to `type = "marginal"` and
  `from_marginals = FALSE` to `type = "normalized"`, with a warning.

## Value

Either a vector with a distribution for a single timestep or a matrix
with a column for each distribution.

## Distribution types

The `type` argument controls how the distribution is computed:

- `"normalized"`:

  The default. Returns the eBird-derived stored distributions,
  normalized so each timestep sums to 1. Equivalent to the previous
  behavior with `from_marginals = FALSE`.

- `"marginal"`:

  Calculates the distribution from the marginals instead of the stored
  distributions. Useful for diagnostics; the two are very similar in
  practice. Equivalent to the previous behavior with
  `from_marginals = TRUE`.

- `"raw"`:

  Returns the eBird abundance values prior to the standardize-to-1
  normalization, by multiplying the stored normalized distribution by
  the per-timestep totals captured during
  [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
  (`x$metadata$abundance$totals`). This requires a model that recorded
  those totals — older models will trigger an error pointing at
  re-preprocessing. Note that quantile trimming via the `trim_quantile`
  argument is also a lossy step: `"raw"` recovers values from before
  normalization but after any trimming.

## See also

Distributions can be passed to
[predict()](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)
or converted to rasters with
[`expand_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/expand_distr.md)
or converted to
[SpatRaster](https://rspatial.github.io/terra/reference/rast.html) with
[`rasterize_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md).
[`sample_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/sample_distr.md)
will convert one cell to 1 and the rest to 0 probabilistically based on
the densities in the distribution.
