# Plot Routes

Plot `Routes` and `BirdFlowRoutes` objects as as lines with color
indicating the passage of time. For `BirdFlowRoutes` the end point of
each week is shown as a dot and the size of the dot corresponds to how
long the birds stayed at that location.

## Usage

``` r
plot_routes(
  routes,
  bf,
  facet = FALSE,
  max_stay_len = NULL,
  use_seasonal_colors = TRUE,
  pal = NULL,
  barheight = 8,
  route_linewidth = 0.85,
  dot_sizes = c(1.1, 3.5),
  coast_linewidth = 0.25,
  stay_units = "weeks",
  show_mask = TRUE,
  crs = NULL,
  static = TRUE
)

# S3 method for class 'BirdFlowRoutes'
plot(x, ...)
```

## Arguments

- routes, x:

  An object of class `Routes` or `BirdFlowRoutes`. Likely the the output
  of
  [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md),
  [as_BirdFlowRoutes](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md),
  or
  [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md).

- bf:

  A BirdFlow object. Only used if `x` is a `Routes` object, in which
  case it provides the CRS and

- facet:

  If `TRUE` then use
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to show each route out into a separate subplot.

- max_stay_len:

  Used to scale the stay length dots. If `NULL` (the default) it will be
  set to the maximum `"stay_len"` value in `routes`. Set it manually to
  keep the dot scaling consistent across multiple plots.

- use_seasonal_colors:

  If `TRUE` a color scale that uses blues, greens, yellows, reds, for
  winter, spring, summer, and fall will be used with a consistent
  mapping of dates to colors regardless of the range of dates plotted.
  If `FALSE` then the data will be plotted using the full color scale.

- pal:

  The color palette to use for plotting when `use_seasonal_cols` is
  `FALSE`. Defaults to [viridisLite::viridis(n =
  5)](https://sjmgarnier.github.io/viridisLite/reference/viridis.html).

- barheight:

  The height of the color gradient legend bar. Passed to
  [`ggplot2::guide_colorbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
  as `barheight` argument. Depending on the output resolution and plot
  size this may need to be adjusted. Can take a number or the output
  from
  [`ggplot2::unit()`](https://ggplot2.tidyverse.org/reference/reexports.html).

- route_linewidth:

  Line width used for routes.

- dot_sizes:

  Two numbers indicating the smallest and largest dot sizes used to
  represent stay length.

- coast_linewidth:

  Line width used for coastlines.

- stay_units:

  The unit to plot the stay length at each location. Default to `weeks`.
  Other options include `sec`, `mins`, `hours`, `days` and `weeks`.

- show_mask:

  Should the BirdFlow Model's (`bf`) static mask be displayed.

- crs:

  Only used when `bf` is missing. `crs` sets the Coordinate Reference
  system used for plotting. See
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

- static:

  For internal use. It is set to `FALSE` when `plot_routes()` is called
  from
  [`animate_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_routes.md).

- ...:

  Passed to `plot_routes()` from
  [`plot()`](https://rspatial.github.io/terra/reference/plot.html) for
  `Route` and `BirdFlowRoutes` objects.

## Value

A ggplot object. Use [`print()`](https://rdrr.io/r/base/print.html) to
display it.

## Details

`plot.BirdFlowRoutes()` calls `plot_routes()`.

## Examples

``` r
bf <- BirdFlowModels::amewoo
n <- 10
rts <- route(bf, n, season = "prebreeding")

# Multiple routes on one plot
plot_routes(rts, bf)


# One panel per route
new_rts <- rts
new_rts$data <- new_rts$data[new_rts$data$route_id %in% 1:4, ]
plot_routes(new_rts, bf, facet = TRUE)


# Returned plot object can be edited
# Here we change the title and add an additional sf
# layer with country boundaries
library(ggplot2)
p <- plot_routes(rts, bf) +
  ggtitle(paste0(species(bf), " (with countries)")) +
  geom_sf(data = get_countries(bf), inherit.aes = FALSE, fill = NA) +
  coord_sf(expand = FALSE)
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
p

if (FALSE) { # \dontrun{
# Use alternate color palettes
plot_routes(rts, bf, use_seasonal_colors = FALSE)

plot_routes(rts, bf,
  use_seasonal_colors = FALSE,
  pal = c("red", "yellow", "blue")
)
} # }
```
