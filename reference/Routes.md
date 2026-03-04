# `Routes()` is used to convert data frames containing bird movement data into a formal `Routes` object with the same data.

`Routes()` is used to convert data frames containing bird movement data
into a formal `Routes` object with the same data.

## Usage

``` r
Routes(data, species = NULL, source = NULL)
```

## Arguments

- data:

  A data frame with data on bird movement. Likely tracking, banding, or
  Motus data on real birds; or possibly synthetic versions of the
  same.It must have the following columns:

  `date`

  :   Date or Date Time object of class `Date`, `POSIXlt`, or `POSIXct`

  `lat`,`lon`

  :   The latitude and longitude of the location in WGS84 (EPSG::4326)

  `route_type`

  :   The type of route - one of `"tracking"`, `"banding"`, `"motus"`,
      `"unknown"`, or `"synthetic"` Types can be mixed in the column.

  Other columns are permitted and will be retained in `Routes` object
  but dropped if they are converted to `BirdFlowRoutes`.

- species:

  Either: a character scalar suitable for use with
  [`ebirdst::get_species()`](https://ebird.github.io/ebirdst/reference/get_species.html);
  or a list with species metadata which must include `common_name` and
  can optionally also include `scientific_name` and `species_code` and
  any other standard BirdFlow species metadata. See
  [`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
  for a description of the full list. Note list input is not checked
  against eBird species codes and names. Scalar input is preferred
  unless the species does not conform to eBird's taxonomy.

- source:

  Optional text describing the source of the data.
  [`source()`](https://rdrr.io/r/base/source.html) must be of class
  `character` can have one or more elements.

## Value

An object of class `Routes` which has the following components

- data:

  A data frame with the input `data`

- species:

  A list with, at a minimum items `common_name`, `scientific_name`, and
  `species_code` and depending on the `species` argument potentially
  having all the items returned by
  [`lookup_species_metadata()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_species_metadata.md)

- source:

  Same as the input `source`
