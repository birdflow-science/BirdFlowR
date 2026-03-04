# Determine BirdFlow model resolution \####

Internal function to determine the resolution to use when creating a
BirdFlow model. It is called by
[`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
and nowhere else, but is complicated enough to justify being it's own
function.

## Usage

``` r
determine_resolution(
  sp_path,
  res,
  max_params,
  gpu_ram,
  clip,
  crs,
  download_species,
  project_method
)
```

## Arguments

- sp_path:

  The species path used with ebirdst to download and load data

- res:

  The target resolution of the BirdFlow model in kilometers. If `res` is
  NULL (default) then a resolution that results in less than
  `max_params` parameters will be used, while also minimizing the
  resolution and limiting the number of significant digits.

- max_params:

  The maximum number of fitted parameters that the BirdFlow model should
  contain. Ignored if `res` is not NULL. Otherwise a resolution will be
  chosen that yields this many fitted parameters. See `gpu_ram` for the
  default way of setting `max_params` and `res`. Note: the reduction in
  parameters resulting from truncation (see `...`) is not factored into
  the calculation.

- gpu_ram:

  Gigabytes of ram on GPU machine that will fit the models. If `res` is
  NULL and `max_params` is NULL this is used to estimate
  `max_params`which is, in turn, used to determine the resolution.
  Ignored if either` res` or `max_params` is set.

- clip:

  A polygon or the path to a file containing a polygon. It must have a
  CRS and should either be a
  [SpatVector()](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  object or produce one when called with
  [vect(clip)](https://rspatial.github.io/terra/reference/vect.html)

- crs:

  Coordinate reference system (CRS) to use. Defaults to the custom
  projection eBird has assigned to this species - see
  [`ebirdst::load_fac_map_parameters()`](https://ebird.github.io/ebirdst/reference/load_fac_map_parameters.html)).
  It will be interpreted by
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html)
  to generate a well known text representation of the CRS.

- download_species:

  The species code used with ebirdst this might be `"example_data"` or
  `"yebsap-example"` but otherwise will be a real species code.

- project_method:

  This is the method used to reproject it is a local variable set within
  `preprocess_species`.

## Value

The resolution in km either as set directly by the user or as derived
from `max_params` or `gpu_ram`.

## Details

When the user specifies a resolution that is the resolution used when
creating the model.

If the `res` argument is `NULL` the heuristic here attempts to set a
from the `gpu_ram` parameter which specifies the the GB of ram available
on the machine used to fit the models.

It turned out to be really hard to anticipate how many cells would
contain data after a resolution change. The code estimates by
calculating the area of the non-zero cells in the current resolution and
then figures out the resolution where the number of cells required to
cover that area matches our target number of parameters.

However, it's a poor estimate because it ignores the fact that coarse
cells along the edges overlap fine cells that contain a mix of no data
and data. reduce the bias.

Next it resamples to the estimated resolution, evaluates the new number
of non-zero cells (and thus parameters), and makes a new estimate,
repeating the process until the realized number of parameters from the
resolution converges on 90 to 100 % of the target. This results in a
fairly precise maximum resolution that can be fit given the number of
parameters.

The last step is to round up (reducing parameters) to a cleaner number.
