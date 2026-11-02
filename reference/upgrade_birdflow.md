# Upgrade an older BirdFlow object to the current in-memory format

Consolidates the back compatibility fixes that were previously scattered
across several functions
([`predict()`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md),
[`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md),
[`route_between()`](https://birdflow-science.github.io/BirdFlowR/reference/route_between.md),
[`predict_between()`](https://birdflow-science.github.io/BirdFlowR/reference/predict_between.md),
[`truncate_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow.md),
and others) into one place. Running `upgrade_birdflow()` on an already
current object is a no-op.

## Usage

``` r
upgrade_birdflow(bf)
```

## Arguments

- bf:

  A BirdFlow object.

## Value

The upgraded BirdFlow object.

## Details

`upgrade_birdflow()`:

1.  Fixes the old `birdFlowr_version` metadata name typo.

2.  Backfills `metadata$timestep_padding` via
    [`get_timestep_padding()`](https://birdflow-science.github.io/BirdFlowR/reference/get_timestep_padding.md)
    if missing.

3.  Backfills metadata fields that are missing entirely (added in later
    versions of the package) with
    [`new_BirdFlow()`](https://birdflow-science.github.io/BirdFlowR/reference/new_BirdFlow.md)'s
    schema defaults, without overwriting any field that's already
    present.

4.  Backfills `dates$week` with `1:52` if missing and the model has 52
    timesteps.

5.  Adds a dynamic mask via
    [`add_dynamic_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/add_dynamic_mask.md)
    if the object doesn't already have one.

## See also

[`add_dynamic_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/add_dynamic_mask.md),
[`validate_BirdFlow()`](https://birdflow-science.github.io/BirdFlowR/reference/validate_BirdFlow.md)

## Examples

``` r
# \donttest{
bf <- upgrade_birdflow(BirdFlowExamples::amewoo)
# }
```
