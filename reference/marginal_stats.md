# marginal statistics

Internal functions to calculate the sum of all marginals and the percent
of marginal values that are zero.

## Usage

``` r
marginal_stats(bf)

sum_marginals(bf)

calc_pct_zero(bf)
```

## Arguments

- bf:

  A BirdFlow model.

## Value

`marginal_stats()` returns a list with

- sum:

  the sum of all the marginals

- pct_zero:

  the percent of the values across all marginals that are zero.

`sum_marginals()` returns the sum of all marginals.

`calc_pct_zero()` returns the percent of marginal values that are zero

## Details

These are used by sparsify and fix_dead_ends. marginal_stats is slightly
more efficient than calling the other two functions independently.
