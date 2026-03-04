# function to determine the threshold in values that preserves a given proportion of the value.

function to determine the threshold in values that preserves a given
proportion of the value.

## Usage

``` r
find_threshold(x, p, method = "weight")
```

## Arguments

- x:

  A vector of values

- p:

  A proportion of x that we wish retain

- method:

  Either `"weight"` to retain a proportion of the total weight or
  `"values"` to retain a proportion of non-zero values.

## Value

A threshold in values of x such that dropping everything smaller than
the threshold retains at least proportion `p` of the total weight or
number of values in `x`
