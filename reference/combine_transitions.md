# Function to combine a sequence of transition matrices into one

Function to combine a sequence of transition matrices into one

## Usage

``` r
combine_transitions(bf, ...)
```

## Arguments

- bf:

  A BirdFlow object

- ...:

  Arguments passed on to
  [`lookup_transitions`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_transitions.md)

  :   

## Value

This returns the transition probabilities associated with a sequence of
timesteps. It will have a column for every unmasked cell at the starting
timestep and a row for every unmasked cell in the last timestep, with
cell values being the probably of transitioning from that row to that
column between the start and end of the time sequence described by `...`
