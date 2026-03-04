# Function to validate a BirdFlow object

Throw an error if a BirdFlow object is malformed or incomplete.

## Usage

``` r
validate_BirdFlow(x, error = TRUE, allow_incomplete = FALSE)
```

## Arguments

- x:

  A BirdFlow object.

- error:

  If TRUE throw an error if there are problems if FALSE return any
  problems as a data.frame.

- allow_incomplete:

  If TRUE allow the BirdFlow object to be missing both marginals and
  transitions (but not other components). This allows checking the
  output of
  [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md).

## Value

If `error = FALSE` the function returns a data.frame describing any
errors with columns:

- problem:

  A character description of any problems.

- type:

  The problem type, either "error" or "incomplete."

Otherwise, if there are no problems a similar data.frame with no rows is
returned invisibly.

## Details

[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
creates a BirdFlow object that lacks both marginals and transitions and
thus can't be used to make projections. `validate_BirdFlow()` tags the
absence of these with the type "incomplete". Any other missing or
malformed components are tagged "error".

Since marginals can be used to calculate both distributions and
transition matrices, a BirdFlow object can be complete if it has
marginals; or has both transitions and distributions. Having redundancy
in these three is not considered an error.

Currently metadata and species information is not checked for
completeness.

Currently dead end transitions are permitted. See
[`find_dead_ends()`](https://birdflow-science.github.io/BirdFlowR/reference/fix_dead_ends.md)
for checking for those.
