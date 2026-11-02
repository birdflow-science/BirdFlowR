# Print BirdFlow models

Print a summary of a `BirdFlow` object: species, raster dimensions,
resolution, number of active cells, and in-memory object size.

## Usage

``` r
# S3 method for class 'BirdFlow'
print(x, ...)
```

## Arguments

- x:

  A BirdFlow object.

- ...:

  arguments passed from other methods

## Value

`x` returned invisibly and unchanged.

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
print(bf)
#> American Woodcock BirdFlow model
#>   dimensions   : 20, 22, 52  (nrow, ncol, ntimesteps)
#>   resolution   : 150000, 150000  (x, y)
#>   active cells : 283
#>   size         : 8.8 Mb
# }
```
