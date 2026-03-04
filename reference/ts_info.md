# Internal function to determine timestep sequence direction and whether it crosses year boundary

It assumes that all lags between adjacent steps that don't cross the
year boundary are either -1 or 1.

## Usage

``` r
ts_info(ts)
```

## Arguments

- ts:

  A sequence of timesteps.

## Value

A list with:

- direction:

  Either `"forward"` or `"backward"`.

- loops:

  `TRUE` if the sequence crosses the year boundary, `FALSE` if it does
  not.
