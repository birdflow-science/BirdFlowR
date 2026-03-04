# Shrink BirdFlow extent

Shrink BirdFlow extent

## Usage

``` r
shrink_birdflow(x)
```

## Arguments

- x:

  A single BirdFlow object, or one or more paths to BirdFlow objects
  stored as either hdf5 or rds files.

## Value

If `x` is an extended BirdFlow model, see
[`extend_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/extend_birdflow.md)`than`shrink_birdflow()\`
returns the same model with its original extent.

If `x` is the path to one or more BirdFlow models than those files are
shrunk to their original extent and a logical vector of the same length
is returned with TRUE for success.

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
