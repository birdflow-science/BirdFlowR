# Lookup timestep

This function returns the timestep or timesteps associated with `x` in a
particular BirdFlow model, where `x` represents dates or timesteps in
various formats.

## Usage

``` r
lookup_timestep(x, bf, allow_failure = FALSE)
```

## Arguments

- x:

  A character object representing date as year-month-day e.g.
  "2023-03-29", date object
  ([`Date`](https://rdrr.io/r/base/Dates.html),
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html), or
  [`POSIXlt`](https://rdrr.io/r/base/DateTimeClasses.html)), a numeric
  timestep, a character representing a timestep e.g. "t1", or "all" for
  all timesteps in the model.

- bf:

  A BirdFlow object.

- allow_failure:

  If TRUE function will return NA values when it fails to resolve a
  timestep for any element of `x`. With the default, FALSE, the function
  will throw an error if not all elements of `x` are resolved to
  timesteps.

## Value

A vector of timesteps corresponding to elements in `x`.

## Details

So far all BirdFlow objects have had timesteps corresponding with weeks
of the year and matching the S&T timesteps. However, it is likely that
we will add the ability to make BirdFlow objects that only model part of
the year. If we do this the timestep values will not necessarily match
weeks. For example a model that covers Week 6, to 20 would have
timesteps from 1 to 15.

If `x` is numeric it is assumed to already be a timestep. This is useful
when using this function internally to resolve arguments to other
functions like
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md),
[`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md),
and
[`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md).

## Examples

``` r
bf <- BirdFlowModels::amewoo
lookup_timestep(c("2001-3-23", "2022-12-05"), bf)
#> [1] 12 49
```
