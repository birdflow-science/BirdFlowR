# Add or drop transition matrices

Given a BirdFlow object with marginals and without transitions
`build_transitions()` return a BirdFlow object with both marginals and
transitions, `drop_tansitions()` will reverse the process.

## Usage

``` r
build_transitions(x, rebuild = FALSE)

drop_transitions(x)
```

## Arguments

- x:

  BirdFlow object

- rebuild:

  Set to TRUE to rebuild transitions if they are already present.

## Value

BirdFlow object with transition matrices

## See also

[`has_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/has.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bf1 <- BirdFlowModels::amewoo
bf2 <- build_transitions(bf)
bf2

bf3 <- drop_transitions(bf2)
bf3
} # }
```
