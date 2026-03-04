# reformat distr time labels based on birdflow_options("time_format")

This is an internal function to change the labels of an object
containing distributions to match the time format the user has specified
in the global options. Internally distributions are stored and generated
with labels based on timestep ("t1" etc.).

## Usage

``` r
reformat_distr_labels(x, bf)
```

## Arguments

- x:

  An object containing bird distributions

- bf:

  A BirdFlow object (used for its date information)

## Value

`x` with (potentially) new labels

## Details

As of March 15, 2023 I'm experimenting with adding an attribute "time"
to a vector distribution to keep track of the time label.

If there are more than one distribution they are stored as a matrix and
the colnames store the time associated with each distribution.

This function currently returns the input object as is if it cannot
resolve labels.
