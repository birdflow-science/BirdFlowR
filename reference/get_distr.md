# Extract distributions from BirdFlow models

`get_distr` will return one or more distributions in their flattened
form. A single distribution will be returned as a vector; if multiple
they will be columns in a matrix.

## Usage

``` r
get_distr(x, which = "all", from_marginals = FALSE)
```

## Arguments

- x:

  A BirdFlow model

- which:

  Indicates which timesteps to return. Can be one or more integers
  indicating timesteps; character dates in the format year-month-day
  e.g. `"2019-02-25"`; [`Date`](https://rdrr.io/r/base/Dates.html)
  objects; or `"all"` which will return distributions for all timesteps.

- from_marginals:

  If TRUE and `x` has marginals the distribution will be from the
  marginals even if `x` also has distributions.

## Value

Either a vector with a distribution for a single timestep or a matrix
with a column for each distribution.

## Details

If the BirdFlow object has stored distributions they will be the
training distributions and will be returned by default unless
`from_marginals = TRUE` in which case distributions calculated from the
marginal will be returned.

The training distributions and the distributions calculated from the
marginal are very similar.

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
