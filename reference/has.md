# Does a BirdFlow object have certain components

These functions report whether a BirdFlow object has certain optional
components:

- `has_marginals()` indicates whether `x` has marginals, the canonical,
  smaller representation of the model's dynamics.

- `has_transitions()` indicates whether `x` has precomputed transitions,
  derived from the marginals, which is what
  [`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)
  and
  [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
  actually multiply against distributions.

- `has_distr()` indicates whether `x` has stored eBird Status & Trends
  distributions, separate from the marginals/transitions, from the data
  the model was trained on.

- `has_dynamic_mask()` indicates whether `x` has a dynamic
  (per-timestep) mask, in addition to the static mask every BirdFlow
  object has.

## Usage

``` r
has_marginals(x)

has_transitions(x)

has_distr(x)

has_dynamic_mask(x)
```

## Arguments

- x:

  A BirdFlow model

## Value

Logical indicating the BirdFlow model has the relevant element

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
has_marginals(bf)
#> [1] TRUE
has_transitions(bf)
#> [1] FALSE
has_distr(bf)
#> [1] TRUE
has_dynamic_mask(bf)
#> [1] TRUE
# }
```
