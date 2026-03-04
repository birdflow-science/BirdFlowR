# export distributions and masks from a BirdFlow object to raster files.

This function exports the distributions and/or dynamic masks from a
BirdFlow object to raster files (TIFF, PNG).

## Usage

``` r
export_rasters(
  bf,
  dir,
  crs = NULL,
  multiband = !singleband,
  singleband = TRUE,
  what = c("distr", "mask"),
  filetype = "GTiff",
  as_integer = NULL,
  factor = NULL,
  overwrite = TRUE,
  mb_file = "<code>_<what>.<ext>",
  sb_file = "<code>_<what>_<p_ts>.<ext>"
)
```

## Arguments

- bf:

  A BirdFlow object

- dir:

  The directory where output should be stored. Can include aliases; see
  `mb_file`,`sb_file` below.

- crs:

  The coordinate reference system to use in the output files, defaults
  to `crs(bf)`

- multiband:

  If `TRUE` export a multiband file. Will be forced to `FALSE` if the
  `filetype` doesn't support multiband rasters.

- singleband:

  If `TRUE` export separate files for each week in the model. Will be
  forced to `TRUE`

- what:

  Either `"distr"`, `"mask"` or both as a two element vector. `what`
  controls what components of `bf` are exported.

- filetype:

  The file type to export: `"GTiff"` for GeoTIFF files, or `"PNG"` for
  Portable Network Graphics.

- as_integer:

  Should the data be written as integers. With the default, `NULL`,
  integers will be written for the PNG file type as they don't support
  real numbers and with GeoTIFFs floating point numbers will be written.

- factor:

  To create integer output (`as_integer = TRUE`) the floating point
  distributions will be multiplied by this number prior to output. If
  `factor` is `NULL` (the default) then the factor will be x / the
  maximum value in any distribution in `bf`, where x is 255 for `PNG`
  filetypes and 1000 for `GTiff`. Thus the maximum integer value will be
  255 for PNG files, and 1000 for GeoTIFF files.

- overwrite:

  Should pre-existing files be overwritten with new output.

- mb_file, sb_file, :

  The multi-band and single-band file name templates. They control where
  files are written. Possible aliases are:

  - `<ext>` the file extension, required at end of template.

  - `<code>` the species code.

  - `<common>` the species common name, spaces will be replaced with
    `"_"`

  - `<scientific>` the scientific name, spaces will be replaced with
    `"_"`

  - `<ts>` timestep (without padding)

  - `<p_ts>` padded timestep e.g. `"03"`

  - `<date>` date in format `year-month-day` e.g. `"2024-03-14"`

  - `<what>` will be one of "distr" or "mask" can be omitted if only one
    is to be output. See `what` argument above.

  The two metadata files (CRS, extent) are written using the multi-band
  template with `<what>` set to `"crs"` and `"extent"`, unless `<what>`
  isn't in the file name in which case `"_crs"` and `"_extent"` will be
  inserted prior to the extension.

  `sb_file` must include one of `<ts>`, `<p_ts>` or `<date>`.

  The aliases above may be used in `dir` as well.

## Value

Nothing is returned, but raster files are written to `dir`

## Details

This replaces and extends the old behavior of
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
which allowed exporting TIFF files of the distributions while
preprocessing.

Files are written via
[terra::writeRaster](https://rspatial.github.io/terra/reference/writeRaster.html)
but the data is manipulated prior to export, in particular if
`as_integer` is `TRUE` or if the filetype only supports integers than
the distribution data which is normally between 0 and 1 is stretched and
then converted to integer. The amount of stretching can be controlled
with `factor`. `as_integer` will be set automatically to TRUE for
formats that only export integers (`PGN`) and `multiband` and
`singleband` will be set to `FALSE` and `TRUE` respectively for formats
that only support single band output (`PNG`). If the `filetype` supports
multiband files than it is possible to export both multiband and single
band by setting both arguments to `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
  bf <- load_model("amewoo_prebreeding")
  dir <- tempdir()
  crs <-"EPSG:4326"
  export_tifs(bf, dir = dir, singleband = TRUE, crs = crs)
  export_tifs(bf, dir = dir, singleband = TRUE, crs = crs, filetype = "PNG")

} # }
```
