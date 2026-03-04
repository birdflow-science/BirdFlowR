# pad timesteps from BirdFlow models

This function is mostly for internal use but exported for advanced
users. It's primary purpose is to pad timesteps with zeros for looking
up transition names. It is called from
[`lookup_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_transitions.md)
which is in turn used by
[`get_transition()`](https://birdflow-science.github.io/BirdFlowR/reference/get_transition.md).
Padding is generally two digit but the level of padding is stored in the
BirdFlow object so that we'll be able to switch to three digit timesteps
easily if, for example, someday we decide to have daily timesteps.

## Usage

``` r
pad_timestep(x, bf)
```

## Arguments

- x:

  A vector of timestep integers

- bf:

  A BirdFlow model

## Value

a string with padded versions of x; `1` becomes `"01"`.

## Examples

``` r
bf <- BirdFlowModels::amewoo
pad_timestep(1:5, bf)
#> [1] "01" "02" "03" "04" "05"
```
