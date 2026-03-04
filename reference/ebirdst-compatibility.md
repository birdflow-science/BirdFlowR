# ebirdst version compatibility functions:

Internal functions to facilitate working with both multiple versions of
of ebirdst despite the changes to the API between 2021 and 2022.

## Usage

``` r
ebirdst_pkg_ver()

ebirdst_ver_supported(use = "package", throw_error = FALSE)

res_label(res)

date_to_week(dates, version = 2022)

ebirdst_example_species()
```

## Arguments

- use:

  The particular use that is being checked against. `package` is whether
  all package functions are supported fully. `preprocess_species` checks
  that function which has stricter requirements than most functions.
  `lookup_species_metadata` checks that function which is called by
  [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md).

- throw_error:

  if `TRUE` an error will be thrown if ebirdst is not supported by
  `use`.

- res:

  A resolution label. One of "lr", "mr", "hr", "27km", "9km", or "3km

- dates:

  a vector of dates that can be processed by
  [`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html)

- version:

  A numeric (year) version of the eBird date scheme to use. 2021 for the
  older or 2022 for the newer; other values will be snapped to the
  closest of those two. The output of
  `ebirdst::ebirdst_version()$version_year` or
  `get_metadata(bf, "ebird_version_year")` is appropriate.

## Value

`ebirdst_pkg_ver()`: The installed ebirdst package version or `NA` if
none.

`ebirdst_ver_supported`: `TRUE` if the specified `use` is supported,
`FALSE` otherwise.

`res_label()`: resolution labels appropriate for installed version of
ebirdst.

`date_to_week()`: A vector of week numbers associated with `dates`

`ebirdst_example_species()`: The example species name for ebirdst

## `ebirdst_pkg_ver()`

`ebirdst_pkg_ver()` Look up the version of the currently installed
ebirdst.

## `ebirdst_ver_supported()`

`ebirdst_ver_supported()` Check whether the installed ebirdst is
supported by BirdFlowR for a particular use.

## `res_label()`

Convert resolution labels so they are appropriate for the installed
ebirdst

ebirdst 3.2022.0 switched from "lr", "mr", and "hr" to "27km", "9km",
and "3km" to indicate low, medium, and high resolution versions of the
raster data in function arguments.
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
uses the older two letter versions but runs them through this function
before calling ebirdst functions.

## `date_to_week()`

This is a slightly modified copy of
[`ebirdst::date_to_st_week()`](https://ebird.github.io/ebirdst/reference/date_to_st_week.html)
that allows calculating weeks from dates without depending on ebirdst.

## `ebirdst_example_species()`

Lookup the example species name that is appropriate for the installed
ebirdst. The example species changed from `"example_data"` to
`"yebsap-example"` in version 3.2022.0.
