# Plot Bird Flow Migration Traffic Rate (BMTR)

DEPRECATED FUNCTION. Please use
[`plot_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_bmtr.md)
instead.

## Usage

``` r
plot_flux(flux, ...)
```

## Arguments

- flux:

  the output from
  [`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)
  or, deprecated,
  [`calc_flux()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_flux.md)

- ...:

  Arguments passed on to
  [`plot_bmtr`](https://birdflow-science.github.io/BirdFlowR/reference/plot_bmtr.md)

  `bf`

  :   A BirdFlow object

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

  `title`

  :   The plot title

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

`plot_bmtr` returns a **ggplot2** object. It can be displayed with
[`print()`](https://rdrr.io/r/base/print.html).
