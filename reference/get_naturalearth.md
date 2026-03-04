# Retrieve, crop, and transform Natural Earth data

These are convenience wrappers to rnaturalearth functions. They
retrieve, crop, format, and project [Natural
Earth](https://www.naturalearthdata.com/) data to facilitate plotting
with BirdFlow and other spatial objects. The output is the desired data
in the same coordinate reference system (CRS) and extent as `x`.

## Usage

``` r
get_naturalearth(
  x,
  type,
  scale = "medium",
  buffer = 15,
  keep_attributes = FALSE,
  country,
  keep_buffer = FALSE,
  force_old_method = FALSE,
  ...
)

get_states(
  x,
  country,
  scale = "medium",
  buffer = 15,
  keep_attributes = FALSE,
  keep_buffer = FALSE
)

get_coastline(
  x,
  scale = "medium",
  buffer = 15,
  keep_attributes = FALSE,
  keep_buffer = FALSE
)

get_countries(
  x,
  scale = "medium",
  buffer = 15,
  keep_attributes = FALSE,
  keep_buffer = FALSE
)
```

## Arguments

- x:

  A BirdFlow,
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  [sf](https://r-spatial.github.io/sf/reference/st_as_sf.html), or any
  other object on which you can call
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)
  and
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

- type:

  The type of data to retrieve. One of "coastline", "country", or
  "states" for data included in rnaturalearth; or any value accepted by
  [ne_download()](https://docs.ropensci.org/rnaturalearth/reference/ne_download.html).

- scale:

  The scale of data to return. Ignored if type is "states", otherwise
  passed to one of
  [ne_download()](https://docs.ropensci.org/rnaturalearth/reference/ne_download.html).
  [ne_coastline()](https://docs.ropensci.org/rnaturalearth/reference/ne_coastline.html),
  or
  [ne_countries()](https://docs.ropensci.org/rnaturalearth/reference/ne_countries.html).
  Valid values are 110, 50, 10, 'small', 'medium', and 'large'.

- buffer:

  A buffer in degrees (latitude and longitude) to add to the extent of
  `x` prior to cropping the Natural Earth data in WGS84. This is needed
  so that after transformation to the CRS of `x` the data cover all of
  the extent of `x`.

- keep_attributes:

  If `FALSE`, the default, attribute columns are dropped to facilitate
  clean plotting.

- country:

  If retrieving states with `get_states()` or
  `get_naturalearth(type = "states")` this is used to select a country.
  If omitted, states from all countries are returned.

- keep_buffer:

  If `FALSE`, the default, after transforming the Natural Earth data it
  will cropped to the precise extent of `x`. Set to `TRUE` to keep the
  buffer - useful when overlaying Natural Earth data on an existing base
  R plot.

- force_old_method:

  This is for internal testing. The default should be best for all other
  uses. If `TRUE` use the back transformed bounding box method even if
  the projection is covered by the "new" cut at seam method.

- ...:

  Other arguments to be passed to
  [ne_download()](https://docs.ropensci.org/rnaturalearth/reference/ne_download.html).
  Possibly you will want to use `category = "physical"`.

## Value

[sf](https://r-spatial.github.io/sf/reference/sf.html) object with
Natural Earth data in the same CRS as `x`.

## Details

`get_naturalearth()` does all the work and is called by the other
functions. There are two distinct calculation methods.

1.  For Mollweide, Lambert Azimuthal Equal Area, Albers Equal Area, and
    Lambert Conformal Conic projections cut at the seam:

    - Find the longitude of projection center ("lon_0" in proj4 string)
      and from it determine longitude of the seam.

    - Clip a narrow (1 m) strip out of the Natural Earth data before
      transforming (in WGS84) at the seam.

    - Transform to the CRS of `x`. This is now an artifact free object
      containing the global data set minus a narrow strip at the seam.

    - Crop in destination to the extent of `x`, or if
      `keep_buffer = TRUE`, the extent plus the approximate equivalent
      of `buffer`.

    This should work well for any extent (including global) in any CRS
    that is based on the covered projections.

2.  For all other projections back transform the bounding box and clip:

    - Convert the corners of the bounds of `x` object to WGS84.

    - adds a buffer (`buffer`) to the converted corners this is
      important to guarantee that we still cover the extent after we
      transform.

    - Check to see if the bounds wrap the seam (180 deg meridian) and
      break the bounding box into two if it does.

    - Crop to the bounding box or boxes.

    - Project each cropped section to `x`'s CRS.

    - Combine the pieces into one object.

    - If `keep_buffer = FALSE` crop to the exact extent of `x`.

    These steps will usually prevent artifacts caused when polygons or
    lines are shifted across the bounds of the CRS. However, it does not
    work for all extents in all projections and in particular is
    unlikely to work with polar projections and with extents that cover
    the entire globe.

    In some cases where this fails setting the buffer to zero may be an
    easy solution.

    There are many more projections where method 1 or a variant on it
    would work. We may eventually cover more of those projections with
    the first method.

    If you encounter a use case that doesn't work you may [submit an
    issue](https://github.com/birdflow-science/BirdFlowR/issues); please
    include the output from `crs(x)` and `ext(x)`.

`get_states()` requires rnaturalearthhires. Install with:  
` install.packages("devtools") # if you don't have it already devtools::install_github("ropensci/rnaturalearthhires") `

## Examples

``` r
 bf <- BirdFlowModels::amewoo
 coast <- get_coastline(bf)

 if (FALSE) { # \dontrun{
 library(terra)
 library(sf)
 plot(rast(bf, 1))
 plot(coast, add = TRUE)   } # }
```
