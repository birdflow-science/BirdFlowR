# Animate BirdFlow Migration Traffic Rate (BMTR)

Animate migration traffic rates produced by
[`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md).

## Usage

``` r
animate_bmtr(bmtr, bf, title = species(bf), ...)
```

## Arguments

- bmtr:

  A data frame created by [calc_bmtr(format =
  "dataframe")](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)

- bf:

  A BirdFlow object

- title:

  The plot title

- ...:

  Arguments passed on to
  [`plot_bmtr`](https://birdflow-science.github.io/BirdFlowR/reference/plot_bmtr.md)

  `subset`

  :   A subset of the transitions in `bmrt` to plot, can be a logical
      vector of the same length as the number of transitions in `bmtr`;
      a numeric index of transitions in `bmtr`, or a subset of the
      transition names in `bmtr`.

  `limits`

  :   Two numbers representing the range in bmtr values to display.
      Values outside of this range will be truncated to the range. With
      the default of `NULL` the entire range is plotted.

  `dynamic_scale`

  :   If `TRUE` then the range of the data in each transition is mapped
      to the color palette. This makes it easier to see the variation
      within a single transition but results in an inconsistent scale
      among transitions.

  `coast_linewidth`

  :   The line width used to plot the coast. Set to `NULL` to skip
      plotting the coastline.

  `coast_color`

  :   The color used to plot the coastline, or `NULL` to skip plotting
      the coastline.

  `gradient_colors`

  :   The colors palette used to represent the BMTR intensity.

  `value_label`

  :   The label for the BMTR values.

  `transform`

  :   A transformation to apply to the color scaling. `"identity"`, and
      `"sqrt"` are recommended. If `"log"` is used zeros will be
      replaced with 1/2 the smallest non-zero value prior to
      transforming. Legend will still reflect the original values.
      Passed to
      [`ggplot2::scale_color_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A [`gganim`](https://gganimate.com/reference/gganimate-package.html)
object

## See also

[`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md),[`plot_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_bmtr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bf <- BirdFlowModels::amewoo
bmtr <- calc_bmtr(bf)

plot_bmtr(bmtr, bf)

animate_bmtr(bmtr, bf)
} # }
```
