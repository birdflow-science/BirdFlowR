# BirdFlowR

<!-- badges: start -->
  [![R-CMD-check](https://github.com/birdflow-science/BirdFlowR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/birdflow-science/BirdFlowR/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

An R package to forecast changes in distributions and generates synthetic 
migration routes based on BirdFlow models.

## Installation

Install the package and the accompanying data package from github:
```{r}
devtools::install_github("birdflow-science/BirdFlowR")
devtools::install_github("birdflow-science/BirdFlowModels")  # data package
```

## Usage 

The two primary functions are `forecast()` to project distributions and code `route()` to generate syntetic routes.  

`route_migration()` is a convience wrapper to `route()` which automates sampling
locations from the starting distribution and setting the start and end dates to 
route over based on the migration window. We can use it to create synthetic 
routes for a species.
```{r}
library(BirdFlowR)
library(BirdFlowModels)

bf <- amewoo 

species(bf)

# start migration at end of the nonbreeding season
start <- get_metadata(bf, "nonbreeding_end")

# Retreive starting distribution as a SpatRaster and plot
r <- rast(bf, start)
plot(r)

# Generate migration rooutes
rts <- route_migration(bf, 10, "prebreeding")

# Add lines to plot
plot(rts$lines, add = TRUE, col = rgb(0, 0, 0, .25))

```

## Learn more

Read  `vignette("BirdFlowR")` for a longer introduction to the package.



