# Fix the dead ends currently present in the model

`fix_current_dead_ends()` zeros out the row or column of the marginal
that leads into current dead ends, eliminating all transitions into the
existing dead end states. It tends to create new dead ends in the
process so is called iteratively by
[`fix_dead_ends()`](https://birdflow-science.github.io/BirdFlowR/reference/fix_dead_ends.md).

## Usage

``` r
fix_current_dead_ends(bf, de)
```

## Arguments

- bf:

  BirdFlow model

- de:

  (optional) output from
  [`find_dead_ends()`](https://birdflow-science.github.io/BirdFlowR/reference/fix_dead_ends.md)

## Value

a BirdFlow model with selected marginal rows and columns zeroed out.
