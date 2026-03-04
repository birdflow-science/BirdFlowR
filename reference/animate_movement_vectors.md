# Animate movement vectors

`animate_movement_vectors()` produces a `gganim` object in which each
frame is a map of vectors showing the average modeled movement for all
birds from each cell in the landscape at a given timestep. It is
analogous to a series of images created with
[`plot_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_movement_vectors.md).

## Usage

``` r
animate_movement_vectors(bf, ...)
```

## Arguments

- bf:

  A BirdFlow object

- ...:

  Arguments passed on to
  [`lookup_timestep_sequence`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)

  `season`

  :   a season name, season alias, or "all". See
      [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
      for options.

  `start`

  :   The starting point in time specified as a timestep, character
      date, or date object.

  `end`

  :   The ending point in time as a date or timestep.

  `direction`

  :   Either "forward" or "backward" defaults to `"forward"` if not
      processing dates. If using date input `direction` is optional and
      is only used to verify the direction implicit in the dates.

  `season_buffer`

  :   Only used with `season` input. `season_buffer` is passed to
      [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
      and defaults to 1; it is the number of timesteps to extend the
      season by at each end.

  `n_steps`

  :   Alternative to `end` The end will be `n_steps` away from `start`
      in `direction`; and the resulting sequence will have `n_step`
      transitions and `n_steps + 1` timesteps.

## Value

A `gganim` object. [`print()`](https://rdrr.io/r/base/print.html) will
plot it with default options, or use
[`gganimate::animate()`](https://gganimate.com/reference/animate.html)
to set the options. See the example for recommended settings.

## Details

Each arrow represents the average of all the transitions from a single
cell. The tail of the arrow is the center of that cell, the head is the
average location at the following timestep for birds that start at that
cell.

The timestep and/or date label is the starting timestep for the
transition that is displayed and the format depends on
[birdflow_options("time_format")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md)

Thicker lines and less transparency (darker shading) indicate higher
density in the eBird S&T distribution for the beginning timestep of the
displayed transition.

Use the "ragg_png" device when rendering animations as in the example
code.

## Examples

``` r
bf <- BirdFlowModels::amewoo
a <- animate_movement_vectors(bf)
#> Creating vector fields
#>  ....................................................

if (FALSE) { # \dontrun{

# Animate, display, and save
#   Note: "ragg_png" is considerably faster and produces cleaner output than
#         the default device.
gif <- gganimate::animate(a, fps = 1, device = "ragg_png",
                          width = 6, height = 5,
                          res = 150, units = "in")
print(gif)

# Save
gif_file <- tempfile("animation", fileext = ".gif")
gganimate::save_animation(gif, gif_file)
file.remove(gif_file) # cleanup
} # }
```
