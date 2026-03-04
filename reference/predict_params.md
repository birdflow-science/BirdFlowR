# predict the number of parameters based on resolution

This function is called by
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
to predicts how many parameters the model is likely to have at a
different resolution given a set of stats on the number of cells and
their area for each timestep at the current resolution, calculated by
[`calc_abundance_stats()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_abundance_stats.md)

## Usage

``` r
predict_params(a_stats, res, adjustment = 0.4)
```

## Arguments

- a_stats:

  output from
  [`calc_abundance_stats()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_abundance_stats.md)
  the only used component is `area` which is a vector of area in square
  meters in the unmasked cells for each timestep.

- res:

  the cell (presumed square) resolution in km.

- adjustment:

  This is used

## Value

The estimated number of parameters given a resolution of `res`

## Details

`predict_params()` calculates a preliminary estimate based on the
inaccurate assumption that the area covered by cells will be the same at
the two resolutions. However, when changing the resolution not all of
the fine cells underlying occupied coarse cells are occupied thus when
increasing the resolution the number of cells are underestimated and
when decreasing the resolution they will be overestimated. The amount of
bias depends on the configuration of occupied cells with more fragmented
occupied areas resulting in greater bias.

`adjustment` allows compensating for this bias. `adjustment` is
multiplied by both the proportional change in resolution: (res2 -
res1)/res1 and by the initial area based estimate and the (possibly
negative) result is added to the area based estimate.

For example if you are doubling the resolution the proportional change
is 1 and with an adjustment of 0.35 you end up adding 35% to the initial
estimate.

Conversely if you are halving the resolution the proportional change is
-.5 and you end up subtracting 17.5% from the estimate.
