# Internal function to make the dates component of a BirdFlow model

Called from
[preprocess_species](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
and not intended for other use.

## Usage

``` r
make_dates(version_year = NULL)
```

## Arguments

- version_year:

  leave NULL for typical usage. Set to a eBird version year to override.
  Used by `switch_date_format()`. version_year \< 2021 will yield old
  date format.

## Value

Dates table appropriate for the current version of ebirdst
