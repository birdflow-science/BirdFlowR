# Animate distributions

Animate distributions as produced by
[`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md),
[`as_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/as_distr.md),
or
[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md).
The distributions will be displayed in the column order in `distr` and
column labels will be used as plot subtitle.

## Usage

``` r
animate_distr(distr, bf, title = species(bf), ...)
```

## Arguments

- distr:

  A set of distributions it should be a matrix with `n_active(bf)` rows
  and a column for each distribution. The animation will proceed in the
  in column order and column names will be used a subtitles in the plot.

- bf:

  The BirdFlow object that `distr` is associated with.

- title:

  The title to use for the animation. The default is the common name of
  the species.

- ...:

  Arguments passed on to
  [`plot_distr`](https://birdflow-science.github.io/BirdFlowR/reference/plot_distr.md)

  `subset`

  :   Defines an optional subset of `distr` that should be plotted. Use
      either column numbers, column names, or a logical vector.

  `show_mask`

  :   If `TRUE` (the default) the static mask that indicates which cells
      are active in the model at any timestep will be shown.

  `show_dynamic_mask`

  :   Defaults to `FALSE`. Set to `TRUE` to visualize the dynamic mask.
      This is achieved by overwriting cells that are dynamically masked
      with NA. For `show_dynamic_mask = TRUE` to work the column names
      in `distr` should all be in `colnames(get_distr(bf))`. This is
      true for distributions returned by
      [[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlowR)
      and
      [`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md).

  `limits`

  :   The range of density values to map `gradient_colors` to. The
      default is the range of the values in `distr` after applying
      `subset`. If you want to standardize the range across multiple
      models of a single species you might want to set to `c(0, max)`
      where `max` is the maximum observed value across all models.
      Alternatively if the range is highly variable among the columns in
      `distr` as when density spreads out from a single point in the
      results of
      [`predict(bf)`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlowR)
      you may want to set this smaller than the full range in which case
      the values will be truncated to the limits (see examples).

  `dynamic_scale`

  :   Set to `TRUE` to have the range of the data in each distribution
      mapped to the full color gradient. This allows visualizing the
      full range of values within each timestep optimally at the cost of
      consistency in the color scale among the facets - or animation
      frames if using `animate_distr()`.

  `coast_linewidth`

  :   The line width to use when plotting the coast. Default is `0.25`.
      If `NULL` the coast will not be plotted.

  `coast_color`

  :   The color to use for plotting the coastline. If `NULL` the coast
      will not be plotted.

  `gradient_colors`

  :   A color gradient that will be used to plot the density values.
      Leave or set to `NULL` for the default of
      `ebirdst::abundance_palette(10, season = "weekly")`.

  `active_cell_color`

  :   The background color for active cells in the landscape. Only used
      if `show_mask` is `TRUE`. These cells will only be visible if
      there are `NA` values in `distr` or if `show_dynamic_mask` is
      `TRUE`.

  `inactive_cell_color`

  :   The color to use for inactive cells in the landscape. These are
      cells that are always masked. Only relevant when
      `show_mask = TRUE`.

  `value_label`

  :   The label used for the values in the distribution. Defaults to
      "Density"

  `transform`

  :   A transformation to apply to the color scaling. Recommended
      `"identity"`, and `"sqrt"`. If `"log"` is used zeros will be
      replaced with 1/2 the smallest non-zero value prior to
      transforming. mapping to the color gradient. Legend will still
      reflect the original values. Passed to
      [`ggplot2::scale_color_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A gganimate object that can be displayed with
[`print()`](https://rdrr.io/r/base/print.html) or or
[`gganimate::animate()`](https://gganimate.com/reference/animate.html).
See example for how to export to a file.

## Examples

``` r
# Animate distributions from BirdFlow object - derived from
# eBird Status and Trends:

bf <- BirdFlowModels::amewoo
ts <- lookup_timestep_sequence(bf, season = "prebreeding")
distr <- get_distr(bf, ts)
anim <- animate_distr(distr, bf,  show_dynamic_mask = TRUE)

if (FALSE) { # \dontrun{
  # Display it
  anim
} # }
### Project a distribution

# Make starting distribution
# Since we define the point in WGS84 (not crs(bf)) we also have to provide
# the crs.
point <- data.frame(x = -90, y = 35)
d1 <- as_distr(point, bf, crs = "EPSG:4326" )

# Project - density will spread over type resulting in a vastly different
# range of values
density_spread <- predict(bf, d1, season = "prebreeding")

# Have the color gradient rescaled to the range of data in each
# individual frame  - density scaling is dynamic.
spread_anim <- animate_distr(density_spread, bf,   dynamic_scale= TRUE)

# Or put in values to use for the limits of the color scale - values outside
# of the limits will be truncated
spread_anim <- animate_distr(density_spread, bf,  limit = c(0, 0.05))


if (FALSE) { # \dontrun{
  # example render fo file
  gif <- gganimate::animate(spread_anim,
                            device = "ragg_png", # fast and pretty
                            width = 7, height = 6,
                            res = 150, units = "in")
  # Display
  print(gif)

  # Save
  gif_file <- tempfile("animation", fileext = ".gif")
  gganimate::save_animation(gif, gif_file)
  file.remove(gif_file) # cleanup
} # }
```
