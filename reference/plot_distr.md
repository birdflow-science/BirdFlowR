# Plot distributions

Return a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object with plots of one or more distributions.

## Usage

``` r
plot_distr(
  distr,
  bf,
  subset = NULL,
  show_mask = TRUE,
  show_dynamic_mask = FALSE,
  limits = NULL,
  dynamic_scale = FALSE,
  coast_linewidth = 0.25,
  coast_color = gray(0.5),
  gradient_colors = NULL,
  active_cell_color = rgb(1, 1, 1, 0.3),
  inactive_cell_color = rgb(0, 0, 0, 0.2),
  title = species(bf),
  value_label = "Density",
  transform = "identity"
)
```

## Arguments

- distr:

  A vector or matrix with distribution values corresponding to active
  cells in `bf`.

- bf:

  A BirdFlow object.

- subset:

  Defines an optional subset of `distr` that should be plotted. Use
  either column numbers, column names, or a logical vector.

- show_mask:

  If `TRUE` (the default) the static mask that indicates which cells are
  active in the model at any timestep will be shown.

- show_dynamic_mask:

  Defaults to `FALSE`. Set to `TRUE` to visualize the dynamic mask. This
  is achieved by overwriting cells that are dynamically masked with NA.
  For `show_dynamic_mask = TRUE` to work the column names in `distr`
  should all be in `colnames(get_distr(bf))`. This is true for
  distributions returned by
  [[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlowR)
  and
  [`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md).

- limits:

  The range of density values to map `gradient_colors` to. The default
  is the range of the values in `distr` after applying `subset`. If you
  want to standardize the range across multiple models of a single
  species you might want to set to `c(0, max)` where `max` is the
  maximum observed value across all models. Alternatively if the range
  is highly variable among the columns in `distr` as when density
  spreads out from a single point in the results of
  [`predict(bf)`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlowR)
  you may want to set this smaller than the full range in which case the
  values will be truncated to the limits (see examples).

- dynamic_scale:

  Set to `TRUE` to have the range of the data in each distribution
  mapped to the full color gradient. This allows visualizing the full
  range of values within each timestep optimally at the cost of
  consistency in the color scale among the facets - or animation frames
  if using
  [`animate_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_distr.md).

- coast_linewidth:

  The line width to use when plotting the coast. Default is `0.25`. If
  `NULL` the coast will not be plotted.

- coast_color:

  The color to use for plotting the coastline. If `NULL` the coast will
  not be plotted.

- gradient_colors:

  A color gradient that will be used to plot the density values. Leave
  or set to `NULL` for the default of
  `ebirdst::abundance_palette(10, season = "weekly")`.

- active_cell_color:

  The background color for active cells in the landscape. Only used if
  `show_mask` is `TRUE`. These cells will only be visible if there are
  `NA` values in `distr` or if `show_dynamic_mask` is `TRUE`.

- inactive_cell_color:

  The color to use for inactive cells in the landscape. These are cells
  that are always masked. Only relevant when `show_mask = TRUE`.

- title:

  The title for the plot. It defaults to the common name of the species
  (`species(bf)`).

- value_label:

  The label used for the values in the distribution. Defaults to
  "Density"

- transform:

  A transformation to apply to the color scaling. Recommended
  `"identity"`, and `"sqrt"`. If `"log"` is used zeros will be replaced
  with 1/2 the smallest non-zero value prior to transforming. mapping to
  the color gradient. Legend will still reflect the original values.
  Passed to
  [`ggplot2::scale_color_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. Use [`print()`](https://rdrr.io/r/base/print.html) to render it.

## Details

### Masks

BirdFlow objects have both a mask and a dynamic mask. For any given
timestep the model only covers cells that aren't excluded by either of
these masks.

- **mask** The static mask, AKA "mask", indicates which cells in the
  raster extent are included in the model at any timestep these are also
  called active cells (e.g.
  [`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)).
  The number of elements in a distribution always match the number of
  unmasked cells in the (static) mask. Cells are masked if they always
  have zero abundance in the Bird S&T data for every week of the year.

- **dynamic mask** The dynamic mask indicates which of the cells not
  excluded by the static mask are modeled within each timestep. It,like
  distributions, only has values for active cells. Thus the dimensions
  of objects returned by
  [get_dynamic_mask(bf)](https://birdflow-science.github.io/BirdFlowR/reference/get_dynamic_mask())
  and get
  [get_distr(bf)](https://birdflow-science.github.io/BirdFlowR/reference/get_distr())
  will be identical. The purpose of the dynamic mask is to improve
  efficiency of fitting, storing, and using BirdFlow models by
  eliminating unlikely locations in the model. The dynamic mask includes
  cells that have a zero in the eBird S&T abundance for the associated
  week.

To display both masks set `show_dynamic_mask` to `TRUE` and leave
`show_mask` at it's default (`TRUE`). Showing the dynamic mask relies on
matching the columns names of the distribution to timesteps in the model
so requires that the column names match the column names in
[`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md).

### Subtitles

Plot subtitles will be derived from the column names in `distr`. It may
be useful to define single distributions as a one column matrix so that
the label information is there. For example if `d` is a distribution in
a matrix with multiple columns use `drop=FALSE` while subsetting,
`plot_distr(d[, 1, drop = FALSE], bf)`; and to add a label to a vector
distribution use `d <- matrix(d, ncol = 1); colnames(d) <- "new label"`

## See also

- [`animate_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_distr.md)
  for animating distributions.

- [`plot_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md)
  and
  [`animate_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_routes.md)
  for visualizing routes.

- [`as_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/as_distr.md),
  [`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md),
  [[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow)
  for functions that produce distributions.

## Examples

``` r
bf <- BirdFlowModels::amewoo
p <- plot_distr(get_distr(bf, c(1,11, 21)), bf, show_dynamic_mask = TRUE)
```
