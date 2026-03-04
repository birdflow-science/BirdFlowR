# Lookup eBird species metadata

`lookup_species_metadata()` uses ebirdst to generate a list identical to
the `species` component of a BirdFlow model. It is an internal function
used by
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
and
[`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md).
See
[`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
for a description of the list items.

## Usage

``` r
lookup_species_metadata(
  species,
  skip_checks = FALSE,
  min_season_quality = 3,
  quiet = FALSE
)
```

## Arguments

- species:

  An eBird species code, common name, or scientific name. It will be
  processed by
  [`ebirdst::get_species()`](https://ebird.github.io/ebirdst/reference/get_species.html).

- skip_checks:

  Set to `TRUE` to skip the checks for eBird model quality and for
  migrant status. Defaults to `TRUE` - do the tests.

- min_season_quality:

  If `skip_checks = FALSE` and the model quality for any of the four
  seasons drops below this threshold an error is thrown.

- quiet:

  Set to `TRUE` to suppress messages.

## Value

A list of species information derived from ebirdst. See
[`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
for a description of the items.

## See also

- [`species()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
  and
  [`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
  for getting species information from a BirdFlow model.

- [`ebirdst::get_species()`](https://ebird.github.io/ebirdst/reference/get_species.html)
  to resolve a species name or code.

- [ebirdst::ebirdst_runs](https://ebird.github.io/ebirdst/reference/ebirdst_runs.html)
  for the source of the information.
