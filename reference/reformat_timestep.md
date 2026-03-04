# reformat timestep labels

given a vector of timestep labels provide a vector of formatted labels
based on the value of
[birdflow_options("time_format")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md)
(see that function for options.)

## Usage

``` r
reformat_timestep(x, bf)
```

## Arguments

- x:

  one (vector) or more (matrix) distributions, with column labels
  consisting of a "t" and the timestep.

- bf:

  A BirdFlow object

## Value

x with update column labels, as dictated by
[birdflow_options("time_format")](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md)

## Details

Internally distributions are labeled with "t" and the timestep integer.
When returning them to the user
[`reformat_distr_labels()`](https://birdflow-science.github.io/BirdFlowR/reference/reformat_distr_labels.md)
is called to change the format which in turn calls this function.
