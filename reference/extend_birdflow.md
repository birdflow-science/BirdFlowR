# Extend BirdFlow extent

Extend BirdFlow extent

## Usage

``` r
extend_birdflow(x, y)
```

## Arguments

- x:

  A single BirdFlow object, or one or more paths to BirdFlow objects
  stored as either hdf5 or rds files.

- y:

  An extent or an object that yields an extent when passed to
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html).

## Value

If `x` is a BirdFlow model object `extend_birdflow()` returns an
extended version of the same model. If `x` is the path to one or more
BirdFlow models than those files are modified and a logical vector of
the same length is returned with TRUE for success.

## Details

Models can only be extended beyond their original extent, however if
they have already been extended it is possible to "extend" them again to
a smaller extent as long as it is also larger than the original extent.

## See also

[`shrink_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/shrink_birdflow.md)
returns a model to it's original extent.

## Examples

``` r
bf <- BirdFlowModels::amewoo

# Define extended extent for example
e <-  ext(bf)
buffer <- 3 * res(bf)
e[1] <- e[1] - buffer[1]
e[2] <- e[2] + buffer[1]
e[3] <- e[3] - buffer[2]
e[4] <- e[4] + buffer[2]

bf2 <- extend_birdflow(bf, e)

bf3 <- shrink_birdflow(bf2)

# Compare extents
data.frame(item = names(as.vector(ext(bf))),
            initial = as.vector(ext(bf)),
            extended = as.vector(ext(bf2)),
            shrunk = as.vector(ext(bf3)))
#>      item  initial extended   shrunk
#> xmin xmin -2550000 -3000000 -2550000
#> xmax xmax  2100000  2550000  2100000
#> ymin ymin -1650000 -2100000 -1650000
#> ymax ymax  1650000  2100000  1650000

if (FALSE) { # \dontrun{
# Plot
library(terra)
plot_distr(get_distr(bf, 1), bf)
plot_distr(get_distr(bf2, 1), bf2)
plot_distr(get_distr(bf3, 1), bf3)
  } # }
```
