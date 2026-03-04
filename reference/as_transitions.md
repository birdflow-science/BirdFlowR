# Convert a sequence of timesteps into a sequence of transition names

Convert a sequence of timesteps into a sequence of transition names

## Usage

``` r
as_transitions(timesteps, bf)
```

## Arguments

- timesteps:

  A valid sequence of timesteps. See
  [`lookup_timestep_sequence()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md).

- bf:

  A BirdFlowR model, used to determine how much padding is needed around
  the timesteps.

## Value

A directional sequence of transitions that connect `timesteps`.
