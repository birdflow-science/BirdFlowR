# Set and retrieve BirdFlowR options

With no arguments all the BirdFlowR options will be returned as a list.
Use a single character value to retrieve the value of a single option.
Use one or more named arguments to set options.

## Usage

``` r
birdflow_options(...)
```

## Arguments

- ...:

  One of: (1) one or more named arguments where the name is a an option
  and the value its new setting e.g. `verbose = FALSE` ; (2) a single
  unnamed argument stating an option to retrieve e.g. `"verbose"` with
  an option to retrieve. (3) No arguments, indicating that all options
  and their current settings should be returned in a list; or (4) a
  single list argument with named items and their new values.

## Value

If no arguments are used than all options will be returned as a list. If
there is a single, unnamed argument with a character value indicating an
option than the value of that option will be returned. Otherwise, the
arguments should indicate new option settings and nothing will be
returned.

## Details

- time_format:

  Indicates what time format to use to label dimensions of distribution
  tables and layers of raster objects returned by
  [`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md),
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  [`rasterize_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md),
  and
  [`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md).
  It does not affect internally stored distribution column labels (which
  are always t1, t2, etc.). Default is "month_day".

  Valid values are: "timestep" which uses the timestep integer appended
  to "t" e.g. "t1"); "date" which uses a date in the format
  year-month-day (as numbers) e.g. "2022-11-23"; and "month_day" which
  uses the name of the month followed by the day of the month e.g.
  "November 23."

- verbose:

  Defaults to `TRUE` for printing of progress and information about the
  process of functions. Set to `FALSE` to turn off printing.

- max_param_per_gpu_gb:

  Controls how many parameters can be fit by BirdFlowPy per gigabyte of
  GPU Ram. This is a conservative estimate based on empirical testing.
  See
  [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)

- cache:

  The local directory to store downloaded model files. Defaults to
  `tools::R_user_dir("BirdFlowR", "data")`. This is the base cache
  directory within which there will be one or more collection specific
  directories, which in turn will hold BirdFlow model files and an
  index.

- collection_url:

  This is the base URL of a collection of model files and its associated
  index. The default is for the standard BirdFlowR model collection.

## Examples

``` r
bf <- BirdFlowModels::amewoo
birdflow_options() # print current settings
#> $cache
#> [1] "/home/runner/.local/share/R/BirdFlowR"
#> 
#> $collection_url
#> [1] "https://birdflow-science.s3.amazonaws.com/collection/"
#> 
#> $max_param_per_gpu_gb
#> [1] 23224801
#> 
#> $time_format
#> [1] "month_day"
#> 
#> $verbose
#> [1] TRUE
#> 
original_format <- birdflow_options("time_format")
birdflow_options(time_format = "date")
head(get_distr(bf, 1:3))
#>       time
#> i      2021-01-04 2021-01-11 2021-01-18
#>   [1,]          0          0          0
#>   [2,]          0          0          0
#>   [3,]          0          0          0
#>   [4,]          0          0          0
#>   [5,]          0          0          0
#>   [6,]          0          0          0
birdflow_options(time_format = original_format)
```
