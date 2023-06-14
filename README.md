
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BirdFlowR <a href="https://birdflow-science.github.io/BirdFlowR/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/birdflow-science/BirdFlowR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/birdflow-science/BirdFlowR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/birdflow-science/BirdFlowR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/birdflow-science/BirdFlowR?branch=main)
<!-- badges: end -->

An R package to predict changes in bird distributions and generate
synthetic migration routes based on BirdFlow models.

This package is under development and not yet formally released.
Function names and arguments may change.

## Installation

Install just the package:

``` r
if(!require("remotes"))
  install.packages("remotes") 
remotes::install_github("birdflow-science/BirdFlowR")
```

Or to install with example data and vignette:

``` r
installed <- rownames(installed.packages())
if(!"remotes" %in% installed)
  install.packages("remotes")
if(!"rnaturalearthdata" %in% installed)
  install.packages("rnaturalearthdata")
remotes::install_github("birdflow-science/BirdFlowModels")
remotes::install_github("birdflow-science/BirdFlowR", build_vignettes = TRUE)
```

See `vignette("Installation")` for troubleshooting and more installation
options.

## Usage

The two primary functions are `predict()` to project distributions and
`route()` to generate synthetic routes.

`route_migration()` is a wrapper to `route()` which automates sampling
locations from the a distribution for the start of the migration and
setting the start and end dates to route for the migration window. We
can use it to create synthetic routes for a species.

``` r
library(BirdFlowR)
library(BirdFlowModels)

bf <- amewoo 

species(bf)
#> [1] "American Woodcock"

# Generate routes for the prebreeding migration 
rts <- route(bf, n = 10, season = "prebreeding")

# Plot routes
plot_routes(rts, bf)
```

<img src="man/figures/README-examples-1.png" width="100%" />

Visualize the movement in the BirdFlow model for a timestep.

``` r

plot_movement_vectors(bf, start = 12)
```

<img src="man/figures/README-movement_vectors-1.png" width="100%" />

## Learn more

- `vignette("BirdFlowR")` has a longer introduction to the package, and
  how to use `predict()` and `route()` to project bird distributions and
  movement.
- `vignette("Installation")` for detailed installation instructions.
- `vignette("Preprocess")` covers downloading and formatting data for
  model fitting with `preprocess_species()`, importing fitted models
  with `import_birdflow()`, and reducing model size with `sparsify()`.  
- Read the paper:
  - [BirdFlow: Learning Seasonal Bird Movements from Citizen Science
    Data](https://www.biorxiv.org/content/10.1101/2022.04.12.488057v1)
    Miguel Fuentes, Benjamin M. Van Doren, Daniel Fink, Daniel Sheldon
    bioRxiv 2022.04.12.488057; doi:
    <https://doi.org/10.1101/2022.04.12.488057>
