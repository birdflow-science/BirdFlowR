# Calculate year offset from a sequence of timesteps

This internal function assigns a year offset for each observation based
on whether the sequence has passed over a presumed year boundary and
started over. The sequence always starts with 0. Forward sequences
increment with each new year, while backwards sequences decrements (0,
-1, etc.). Elsewhere there are limitations on
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
that prevent routes longer than 1 full year (back to start), but this
function does not have that limit.

## Usage

``` r
calc_year_offset(x)
```

## Arguments

- x:

  a sequence of integers representing timesteps.

## Value

sequence of year offsets these will be 0 or 1 for forward sequences and
0 or -1 for backwards.

## Details

It is used by
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
to convert from circular to linear time by making dates monotonic.
