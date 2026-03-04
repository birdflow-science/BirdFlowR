# Prepare eBird Status and Trends data for BirdFlow model fitting

Write a template BirdFlow object to an hdf5 file based on distribution
data downloaded with ebirdst. The object is complete except for
marginals and transitions. Use `...` to truncate the model to part of
the year.

## Usage

``` r
preprocess_species(
  species = NULL,
  out_dir = NULL,
  res = NULL,
  hdf5 = TRUE,
  overwrite = TRUE,
  crs = NULL,
  clip = NULL,
  max_params = NULL,
  gpu_ram = 12,
  skip_quality_checks = FALSE,
  min_season_quality = 3,
  trim_quantile = NULL,
  ...
)
```

## Arguments

- species:

  A species in any format accepted by
  [`ebirdst::get_species()`](https://ebird.github.io/ebirdst/reference/get_species.html)

- out_dir:

  Output directory, files will be written here. Required unless `hdf5`
  is FALSE. File names created here will incorporate the species code,
  resolution, and eBird version year.

- res:

  The target resolution of the BirdFlow model in kilometers. If `res` is
  NULL (default) then a resolution that results in less than
  `max_params` parameters will be used, while also minimizing the
  resolution and limiting the number of significant digits.

- hdf5:

  If TRUE (default) an hdf5 file will be exported.

- overwrite:

  If TRUE (default) any pre-existing output files will be overwritten.
  If FALSE pre-existing files will result in an error.

- crs:

  Coordinate reference system (CRS) to use. Defaults to the custom
  projection eBird has assigned to this species - see
  [`ebirdst::load_fac_map_parameters()`](https://ebird.github.io/ebirdst/reference/load_fac_map_parameters.html)).
  It will be interpreted by
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html)
  to generate a well known text representation of the CRS.

- clip:

  A polygon or the path to a file containing a polygon. It must have a
  CRS and should either be a
  [SpatVector()](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  object or produce one when called with
  [vect(clip)](https://rspatial.github.io/terra/reference/vect.html)

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

- skip_quality_checks:

  If `TRUE` than preprocess the species even if not all of four ranges
  are modeled (under ebirdst 2021 version year) or for 2022 and
  subsequent data versions if not all `<season>_quality` is higher than
  `min_season_quality` in
  [ebirdst_runs](https://ebird.github.io/ebirdst/reference/ebirdst_runs.html)).

- min_season_quality:

  The minimum acceptable season quality when preprocessing eBird 2022
  and subsequent versions. Used to check model quality using based on
  the four `<season>_model_quality` columns in
  [ebirdst_runs](https://ebird.github.io/ebirdst/reference/ebirdst_runs.html)
  ignored with 2021 ebirdst version year.

- trim_quantile:

  With the default of `NULL` there is no outlier trimming, otherwise a
  single value between 0 and 1 to indicate the quantile to truncate at
  or a series of 52 such values corresponding with each week. Trimming
  outliers is always done week by week with the values above the
  `trim_quantile` quantile set to the value of that quantile. Reasonable
  non `NULL` values will be close to 1 e.g. 0.99, 0.995, 0.999. Set
  `trim_quantile` to eliminate high outliers that are believed to be
  model artifacts. See [Issue
  \#189](https://github.com/birdflow-science/BirdFlowR/issues/189) for
  detailed justification.

- ...:

  Arguments passed on to
  [`lookup_timestep_sequence`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)

  `season`

  :   a season name, season alias, or "all". See
      [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
      for options.

  `start`

  :   The starting point in time specified as a timestep, character
      date, or date object.

  `end`

  :   The ending point in time as a date or timestep.

  `direction`

  :   Either "forward" or "backward" defaults to `"forward"` if not
      processing dates. If using date input `direction` is optional and
      is only used to verify the direction implicit in the dates.

  `season_buffer`

  :   Only used with `season` input. `season_buffer` is passed to
      [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
      and defaults to 1; it is the number of timesteps to extend the
      season by at each end.

  `n_steps`

  :   Alternative to `end` The end will be `n_steps` away from `start`
      in `direction`; and the resulting sequence will have `n_step`
      transitions and `n_steps + 1` timesteps.

## Value

Returns a BirdFlow model object that lacks marginals, but is otherwise
complete. It is suitable for fitting with
[BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy).

## Maximum number of parameters

The maximum number of parameters that can be fit is machine dependent.
2023-02-10 we tested under different resolutions with "amewoo" and
identified bounds on the maximum.

|            |              |                      |                      |                 |
|------------|--------------|----------------------|----------------------|-----------------|
| Machine    | GPU Ram (GB) | Lower Bound (worked) | Upper Bound (failed) | Parameters / GB |
| titanx GPU | 12GB         | 306804561            | 334693725            | 25567047        |
| m40 GPU    | 24GB         | 557395226            | 610352178            | 23224801        |

The number of parameters is the number of unmasked cells for the first
timestep + the total number of cells in the marginals which is
calculated from the dynamic mask.

If `gpu_ram` is used (and not `res` or `max_parameters` ) than
`max_parameters` is set to `23,224,801 * gpu_ram` (lower of two values
in table above).

The heuristic to determine resolution given a maximum number of
parameters must estimate the number of cells covered by the data at a
different resolution, a noisy process, so it iteratively tries to find
the smallest resolution that doesn't exceed `max_params` and then rounds
to a slightly larger resolution (fewer parameters).

## Examples

``` r
if (FALSE) { # \dontrun{

 bf <- preprocess_species("amewoo", hdf5 = FALSE )
 plot_distr(get_distr(bf, c(1, 26)), bf = bf)

# Create clip polygon as an sf object
# Use the extent rectangle but with western edge moved in
# The clip can be anything that terra::vect will process into a polygon
e <- ext(bf)
e[1] <- -1500000
coords <- matrix(c(e[1], e[3],
                   e[1], e[4],
                   e[2], e[4],
                   e[2], e[3],
                   e[1], e[3]), ncol = 2, byrow = TRUE)
sfc <- sf::st_sfc(sf::st_polygon(list(coords)), crs = crs(bf))
clip <- sf::st_sf(data.frame(id = 1, geom = sfc))

bfc <- preprocess_species("amewoo", hdf5 = FALSE, clip = clip ) # with clip

 plot_distr(get_distr(bfc, 1), bfc)


} # }
```
