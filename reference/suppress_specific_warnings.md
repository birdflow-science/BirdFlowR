# Suppress warnings that match one or more regular expressions

`suppress_specific_warnings()` will suppress warnings that match regular
expression patterns that are supplied via the `patterns` argument,
without suppressing warnings that don't match the patterns.

## Usage

``` r
suppress_specific_warnings(x, patterns = NULL)
```

## Arguments

- x:

  An expression.

- patterns:

  One or more patterns to check warning messages against.

## Value

Possibly output from `x`
