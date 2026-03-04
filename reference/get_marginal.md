# Return a marginal matrix from a BirdFlowR model

Marginals in BirdFlow models are stored such that the cell \[i, j\]
represents the probability of the bird being in state i in the prior
timestep and state j in the next. Thus the number of rows in the
marginal equals the number of cells within the dynamic mask for the
prior timestep and the columns count is equal to the included cells for
the following timestep.

## Usage

``` r
get_marginal(x, marginal = NULL, from = NULL)
```

## Arguments

- x:

  A BirdFlow object

- marginal:

  A marginal code, e.g. "M_01-02"

- from:

  The first timestep associated with the marginal. Note marginals are
  always forward so the second marginal will be `from + 1` or `1` (when
  `from` is the last timestep).

## Value

A marginal matrix

## See also

[`lookup_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_transitions.md)
will generate a list of the transitions needed to predict or route
between two points in time.
[`get_transition()`](https://birdflow-science.github.io/BirdFlowR/reference/get_transition.md)
will return a transition matrix - often calculated on the fly from a
marginal.
