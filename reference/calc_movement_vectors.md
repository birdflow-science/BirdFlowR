# Calculate the average movement for cells in a BirdFlow model

`calc_movement_vectors()` calculates the average modeled movement of
birds from each location over a single transition in a BirdFlow model.

## Usage

``` r
calc_movement_vectors(bf, start, direction = "forward")
```

## Arguments

- bf:

  A BirdFlow model

- start:

  The starting timestep for the transition to be modeled

- direction:

  "Forward" by default. Set to "backward" to calculate vectors for a
  transition backwards in time.

## Value

A data frame with columns:

- i:

  The location index of the starting location. This is the row in the
  distribution matrix that corresponds to the location.

- start:

  The starting timestep of the transition.

- end:

  The ending timestep of the transition.

- start_x:

  The x coordinate of the starting cell of the transition.

- start_y:

  The y coordinate of the starting cell of the transition.

- end_x:

  The weighted average destination x coordinate of all transitions from
  the starting cell, with weights set to the transition probability for
  each destination.

- end_y:

  The weighted average destination y coordinate.

- weight:

  This is the proportion of the population at the starting cell in the
  eBird S&T distribution for the starting timestep.

- width:

  This is a range rescaling of weight and is used by both
  [`plot_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_movement_vectors.md),
  and
  [`animate_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_movement_vectors.md)
  to set the line width of the arrows.

## Details

In practice each row of the transition matrix represents a single
starting location (`start_x` and `start_y`) and the values in the row
represent transition probability to a number of destination cells. The
probabilities are used as weights to calculate an average of both the
destination x and y coordinates (`end_x`, and `end_y`). The ending
coordinates of the arrow represented the expected average destination of
all birds that start in the cell.

## See also

[`plot_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_movement_vectors.md)
and
[`animate_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_movement_vectors.md)
call this function and visualize the results.

## Examples

``` r
bf <- BirdFlowModels::amewoo
mv <- calc_movement_vectors(bf, 7)
```
