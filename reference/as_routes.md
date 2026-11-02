# Convert a data frame into a `Routes` object

`as_routes()` converts data frames containing bird movement data into a
formal `Routes` object with the same data.

## Usage

``` r
as_routes(data, species = NULL, source = NULL)
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

  :   The type of route. Standard values are `"tracking"`, `"banding"`,
      `"motus"`, `"unknown"`, and `"synthetic"`, but any character value
      is accepted; non-standard values produce a warning. Types can be
      mixed in the column.

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

  Optional text describing the source of the data. `source` must be of
  class `character` can have one or more elements.

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

## See also

[`as_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_routes.md)
for converting `Routes` to `BirdFlowRoutes`.

## Examples

``` r
# \donttest{
df <- data.frame(
  route_id = c(1, 1, 2, 2),
  date = as.Date(c("2021-01-04", "2021-01-11",
                    "2021-01-04", "2021-01-11")),
  lon = c(-87.6, -87.2, -85.0, -84.6),
  lat = c(35.8, 36.0, 33.5, 33.8),
  route_type = "tracking"
)
rts <- as_routes(df, species = "amewoo")
rts
#> --------------------------------------------- 
#> Routes Object 
#> 
#> Species:          American Woodcock / Scolopax minor / amewoo
#> Type:             tracking
#> Number of routes: 2
#> Number of points: 4
#> Date range:       2021-01-04, 2021-01-11
#> Longitude range:  -87.6000, -84.6000
#> Latitude range:   33.5000, 36.0000
#> --------------------------------------------- 
#>   route_id       date   lon  lat route_type
#> 1        1 2021-01-04 -87.6 35.8   tracking
#> 2        1 2021-01-11 -87.2 36.0   tracking
#> 3        2 2021-01-04 -85.0 33.5   tracking
#> 4        2 2021-01-11 -84.6 33.8   tracking
# }
```
