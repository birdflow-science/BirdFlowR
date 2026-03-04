# Animate routes

Animate synthetic routes produced by
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
and
[`route_migration()`](https://birdflow-science.github.io/BirdFlowR/reference/route_migration.md),
producing a dynamic version of
[`plot_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md).

## Usage

``` r
animate_routes(routes, bf, ...)
```

## Arguments

- routes:

  An object of class `Routes` or `BirdFlowRoutes`. Likely the the output
  of
  [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md),
  [as_BirdFlowRoutes](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md),
  or
  [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md).

- bf:

  A BirdFlow object

- ...:

  Arguments passed on to
  [`plot_routes`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md)

  `facet`

  :   If `TRUE` then use
      [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
      to show each route out into a separate subplot.

  `max_stay_len`

  :   Used to scale the stay length dots. If `NULL` (the default) it
      will be set to the maximum `"stay_len"` value in `routes`. Set it
      manually to keep the dot scaling consistent across multiple plots.

  `use_seasonal_colors`

  :   If `TRUE` a color scale that uses blues, greens, yellows, reds,
      for winter, spring, summer, and fall will be used with a
      consistent mapping of dates to colors regardless of the range of
      dates plotted. If `FALSE` then the data will be plotted using the
      full color scale.

  `pal`

  :   The color palette to use for plotting when `use_seasonal_cols` is
      `FALSE`. Defaults to [viridisLite::viridis(n =
      5)](https://sjmgarnier.github.io/viridisLite/reference/viridis.html).

  `barheight`

  :   The height of the color gradient legend bar. Passed to
      [`ggplot2::guide_colorbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
      as `barheight` argument. Depending on the output resolution and
      plot size this may need to be adjusted. Can take a number or the
      output from
      [`ggplot2::unit()`](https://ggplot2.tidyverse.org/reference/reexports.html).

  `route_linewidth`

  :   Line width used for routes.

  `coast_linewidth`

  :   Line width used for coastlines.

  `dot_sizes`

  :   Two numbers indicating the smallest and largest dot sizes used to
      represent stay length.

  `stay_units`

  :   The unit to plot the stay length at each location. Default to
      `weeks`. Other options include `sec`, `mins`, `hours`, `days` and
      `weeks`.

  `show_mask`

  :   Should the BirdFlow Model's (`bf`) static mask be displayed.

  `crs`

  :   Only used when `bf` is missing. `crs` sets the Coordinate
      Reference system used for plotting. See
      [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

  `static`

  :   For internal use. It is set to `FALSE` when
      [`plot_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md)
      is called from `animate_routes()`.

## Value

A `gganim` object. [`print()`](https://rdrr.io/r/base/print.html) will
plot it with default options, or use
[`gganimate::animate()`](https://gganimate.com/reference/animate.html)
to set the options. See the example for recommended settings.

## Details

Note when rendering early frames (at a minimum the first) there will
only one point per route, resulting in a message: "geom_path(): Each
group consists of only one observation. ℹ Do you need to adjust the
group aesthetic?" This will possibly be repeated while individuals
remain in one location. It can be safely ignored. The error is thrown
while rendering and not from within `animate_routes()` so cannot be
suppressed by code in BirdFlowR.

## Examples

``` r

bf <- BirdFlowModels::amewoo
rts <- route(bf, 10,  season = "prebreeding")
anim <- animate_routes(rts, bf)

if (FALSE) { # \dontrun{
  # example render
  timesteps <- unique(rts$data$timestep)
  gif <- gganimate::animate(anim,
                            device = "ragg_png", # is fast and pretty
                            width = 7, height = 6,
                            res = 150, units = "in",
                            nframes = length(timesteps) * 4, fps = 8)

  # Display
  print(gif)

  # Save
  gif_file <- tempfile("animation", fileext = ".gif")
  gganimate::save_animation(gif, gif_file)
  file.remove(gif_file) # cleanup
} # }
```
