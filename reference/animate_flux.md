# Animate Bird Flow Migration Traffic Rate (BMTR)

DEPRECATED FUNCTION. Please use
[`animate_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_bmtr.md)
instead.

## Usage

``` r
animate_flux(flux, ...)
```

## Arguments

- flux:

  the output from
  [`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)
  or, deprecated,
  [`calc_flux()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_flux.md)

- ...:

  Arguments passed on to
  [`animate_bmtr`](https://birdflow-science.github.io/BirdFlowR/reference/animate_bmtr.md)

  `bf`

  :   A BirdFlow object

  `title`

  :   The plot title

## Value

A [`gganim`](https://gganimate.com/reference/gganimate-package.html)
object
