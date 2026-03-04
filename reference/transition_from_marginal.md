# convert a marginal into a transition matrix

internal function to generate a transition matrix from a marginal

## Usage

``` r
transition_from_marginal(m, direction)
```

## Arguments

- m:

  a marginal

- direction:

  the desired transition direction, either "forward" or "backward"

## Value

a transition matrix formulated such that you multiply the matrix by a
distribution to project the distribution. See
[`get_transition()`](https://birdflow-science.github.io/BirdFlowR/reference/get_transition.md)
for more details.

## Details

this is called from
[`get_transition()`](https://birdflow-science.github.io/BirdFlowR/reference/get_transition.md).
If at some point we decide to store transitions rather than marginals it
will also be called from
[`import_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md).
