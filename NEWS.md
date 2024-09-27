# BirdFlowR 0.1.0.9068
2024-09-24

Move `build_collection_index()` to **BirdFlowPipeline**



# BirdFlowR 0.1.0.9067
2024-07-24

## Probabilistic flux

**Note** this version adds the machinery for calculating probabilistic flux but
the parameterization is not yet right and it will likely need some optimization
to run on non-trivial models. 

* Add `weight_betweeness()` which returns betweenness weights in a manner similar
to `is_between()`.
* Add `calc_distance_weights()`,`calc_martern_variance()` and some related 
helper functions that support `weight_betweeness()`
* Update `calc_flux()` with argument `weighted` which switches between
 the old binary betweenness with linear paths and the new probabilistic flux 
 with weighted betweenness in which the probability spreads in the middle of the 
 transition path and spreads more for longer paths.  
* Add `data-raw/martern_parameters.R` which helps visualize how parameters and
functional forms affect weights.

# BirdFlowR 0.1.0.9066
2024-07-11

Add `plot_loss()` to plot the change in the total loss and three loss 
components during fitting. It relies on metadata added to the HDF5 file by
BirdFlowPy.  

# BirdFlowR 0.1.0.9065
2024-06-14

Add `trim_quantile` argument to `preprocess_species()` and `process_rasters()`
to allow trimming values above a quantile in the values to the quantile's value.
E.g. with `trim_quantile = 0.99` any value above the 99th percentile will be
trimmed to value of the 99th percentile.

The motivation is to trim high outliers when they are believed to be artifacts
of the S&T models and eBird data. Robins are a good example. 

Closes #198 "Outliers". 

# BirdFlowR 0.1.0.9064
2024-06-14

Fix #191  Date lookup now works for seasons that start and end on the same day.

# BirdFlowR 0.1.0.9063
2024-06-12

Fix #168 -- allow `NA` in species date fields as in Magnificent Frigate bird.

* Replace `NA` with `$species` with empty string before writing to HDF5 with
`export_birdflow()`
* Replace empty string with `NA` when reading `$species` items in 
`import_birdflow()`.
* Throw meaningful error within `lookup_season_timesteps()` when the required
date in `bf$species` is `NA`. This affects lots of functions that allow
specifying date ranges with `season = `.  This includes `predict()`, `route()`,
`lookup_timestep_sequence()`, and `truncate_birdflow()`.


# BirdFlowR 0.1.0.9062
2024-06-07

* Updated code coverage github action in attempt to fix broken upload to 
codecov.io despite the coverage action running and generating the coverage data.


# BirdFlowR 0.1.0.9061
2024-06-05 

* Add `suppress_specific_warnings()` internal function.
* Update `plot_distr()`, `plot_route()`, and `plot_flux()` 
so that they work with BirdFlow models in which the extent does not overlap 
the coast. `bcrfin`, Brown-capped Rosy-Finch is one example. 
* Add `transform` argument to `plot_flux()` and `plot_distr()` to allow 
log (`"log"`) and square route (`"sqrt"`) transformations prior to applying 
the color scale.  These allow differentiating the smaller differences better.
I think the square route transformation might be the way to go.



# BirdFlowR 0.1.0.9060
2024-05-15 

* Update `preprocess_species()` `crs` argument so that it accepts objects 
of class `crs` as produced by [sf::st_crs()].
* Fix bug in `preprocess_species()` that prevented clipping to irregular 
  polygons.


# BirdFlowR 0.1.0.9059
2024-05-15

* Added `callaghan_abundance` dataset on species populations.
* Added `get_population()`  function.                 

# BirdFlowR 0.1.0.9058
2024-05-07

* Fixed bug that prevented knitting example collection index without passed
parameters.

# BirdFlowR 0.1.0.9057
2024-04-17

* `import_birdflow()` now works with preprocessed hdf5 files. See #177
* `preprocess_birdflow()` and `import_birdflow()` are now more consistent 
  with some dimension names:
    * `bf$geom$ext` is now an unnamed vector in objects from either function.
    Previously one was a named vector. The `ext(bf)` return object is unchanged.
    * `distr`, `dynamic_mask`, `uci`, and `lci` now always have `NULL` 
    rownames and 
    `"t<timestep>"` column names, and the names
    of those two dimensions are `"i"` and `"time"`.
    * `import_birdflow()` no longer drops two metadata items that it was losing
    before: `ebirdst_version`  and `birdflowr_preprocess_version` these 
    will exist in old `.hdf5` files but not in old `.rds` files.  
* `compare_lists()` (internal helper) return format changed slightly.
* `import_birdflow()` and `export_birdflow()` now work with sparse models.
   Marginals from these models are converted to standard matrices before 
   writing so some of the benefits of sparsification are lost when writing to
   hdf5 files - but compression will probably mitigate this somewhat. Sparse
   Matrices are re-created when sparse models are re-imported.
   

# BirdFlowR 0.1.0.9056
2024-04-04

## Flux III 
* `is_between()` now uses a `SparseArray::SparseArray()` for the logical array.
  to reduce memory usage
* Labeling updates to `plot_flux()` and `animate_flux()`.


# BirdFlowR 0.1.0.9055
2024-03-29

## Flux II

* Flux values are now divided by the radius to standardize units at
  at P/km/week/  where P is the proportion of the population.
* Add tests for `calc_flux()` and `is_between()`
* Add analysis of flux radius parameter's affect on flux values. It's in 
 `"test-calc_flux.R"`.
* Add limit to radius argument in `calc_flux()` and ability to bypass it with
  `check_radius` argument.
* Modified `is_between()` to only calculate the result for transitions that
are non-zero (at any timestep) in `bf`.  This provides a slight performance
benefit for standard models and a large one for sparse models. 
* `is_betweeen()` now processes connection lines in batches, controlled by
 `batch_size` argument to it and `calc_flux()`.  This is for memory management.


# BirdFlowR 0.1.0.9054
## Flux I
Non-directional only, additional changes pending

### Changes

* The first argument of `lookup_date()` is now `x` instead of `timestep` 
  `timestep` is added as a deprecated third argument so all old usage should 
  work.  
* `lookup_dates()` now works with transition names (e.g. "T_01-02") marginal 
names ("M_02-03"), character timesteps ("T1", "T2"), and, as before, numeric 
timesteps. Marginal and transition dates are the midpoint between the two 
timesteps, this falls squarely between two dates and it is left up to 
`base::mean.Date()` to resolve this, which it does by rounding down.


* Add internal function `is_between()` to determines if a set points are on the
great circles connecting every possible pair of active cells in a BirdFlow 
model.  It will be used to calculate flux. Currently it is non-directional.

* Add  `get_marginal()`  
* Add `calc_flux()`, `plot_flux()`, `animate_flux()`  currently they only 
calculate and work with non-directional flux / net movement.


### Pending flux changes

* Test `is_between()` 
* Test `calc_flux()`
* Test `get_marginal()`
* Test `plot_flux()` and `animate_flux()`

* Normalize?  See Dan's comment in the issue.
* Directional `is_between()`
* Directional `calc_flux()`


# BirdFlowR 0.1.0.9053
2024-03-15

* New `shrink_birdlfow()` reverses `extend_birdflow()`, returning the model
  to it's original extent.
* `extend_birdflow()`now can "extend" BirdFlow models to smaller extents 
  if they have already been extended and the new extent is still larger than 
  the original. 

# BirdFlowR 0.1.0.9052
2024-03-14

* `export_birdflow()` gets new arguments to control output file names. 
  Default values mimic old behavior.


# BirdFlowR 0.1.0.9051
2024-02-28

* Add `get_mask()`
* Add `extend_bird_flow()`
* Add internal `read_geom()`
* Add internal `extend_geom()`
* Make `export_birdflow()` a public function.
* Update `import_birdflow()` so that it can re-import a BirdFlow model.
  Previously the only viable workflow was `preprocess_birdflow()`, fit model
  in BirdFlowPy, and then`import_birdflow()` (once).  After that we wrote the 
  model to .rda files.  Now it's possible to do the above but then save it 
  to a new `hdf5`  with: `export_birdflow()` (to hdf5) and then import again 
  with `import_birdflow()`.   Note, however, that the file structure of a 
  re-exported hdf5 will not match that of the output from python, b/c on the 
  first import R updates a bunch of data structures - for instance renaming the
  marginals and adding a marginal index, and the re-exported model retains those
  updates.  Thus, the need to modify  `import_birdflow()` to behave 
  appropriately with both file structures.

# BirdFlowR 0.1.0.9050
2024-02-22

* Add test for `preprocess_species()` with a non-default CRS.

# BirdFlowR 0.1.0.9049
2024-01-11

#### Drop "_clip"

 Drop "_clip" from name when clipping with preprocess_species() 
See issue #165, but note I haven't added the extra metadata item yet.  

#### Zero abundance for some timesteps

 `validate_birdflow()` and thus also `preprocess_species()` now throw errors
if any distribution doesn't sum to 1 or if the dynamic mask excludes all cells
for a timestep. This addresses a problem discovered when attempting to fit 
models with species: "antnig" (Antillean Nighthawk) where for some timesteps
the abundance grids from status and trends are all zero.  Prior to this change
it was possible to preprocess Antillean Nighthawk but then during fitting with
BirdFlowPy confusing errors were thrown.  

The change can be observed with:
```{r}
preprocess_species("antnig", res = 150, hdf5 = FALSE, 
                    skip_quality_checks = TRUE)`
```



# BirdFlowR 0.1.0.9048
2023-12-22

* Fix `snap_to_birdflow()` 
  * Now works with date column names other than "date"
  * Consistently re-formats dates with `lubridate::as_date()` and standardizes
  return date column to "date" 
  * now handles empty data frame input gracefully by returning
  empty data frame with appropriate column names and classes.
  
* Fix `export_rasters()` bugs when exporting TIFF files (with some combinations
  of arguments)

# BirdFlowR 0.1.0.9047
2023-12-22

* Fix `snap_to_birdflow()` now works with preprocessed (but not fit) models.

# BirdFlowR 0.1.0.9046
2023-12-21

New functions:

* `snap_to_birdflow()` snap movement data to a BirdFlow model 
week and cells and optionally aggregate the data from each week 
(by unique bird or track).  

* `lookup_date()` convert timesteps to dates.

Changes

* `lookup_timestep()` gains a new argument `allow_failure`.  FALSE, the default
  mimics prior behavior.  Set to TRUE to return NA if the date cannot be 
  looked up.

# BirdFlowR 0.1.0.9045
2023-12-18

New function `combine_transitions()` combines the transition matrices from a 
series of timesteps into a single transition matrix that encodes the transition
probabilities between the starting and ending timesteps in a single multi-week
transition matrix.  

New  **BirdFlowExtras** package uses `combine_transitions()` to calculate
migratory connectivity.  That package will hold more esoteric uses of 
BirdFlow models, and any functions that rely on packages that are not on either
CRAN or Bioconductor. 

# BirdFlowR 0.1.0.9044
2023-12-16

More test tweaking - all changes should be invisible to users.

* Sped up tests, mostly by using truncated models for slow tests.
* Added helper functions to `tests/testthat/`:
    * `local_quiet()` - Sets verbose to FALSE within a test scope
    * `local_test_dir()` - Creates a temporary directory that will be deleted 
      automatically on test completion.
* Eliminated almost all messages output during testing to improve test result 
  legibility.

# BirdFlowR 0.1.0.9043
2023-12-15

More tests and cleaner message handling.
Users should be unaffected, but might see fewer messages if 
`birdflow_options("verbose")` is `TRUE`.

* New test for `build_collection_index()`
* New internal `bf_msg()` now used for messages from BirdFlowR functions.
* New internal `bf_suppress_msg()` to wrap calls to other packages so that 
  their messages are suppressed when verbose is false.


# BirdFlowR 0.1.0.9042
2023-12-14

* Updated sparsification to work with dynamically masked models
* Added new parameter `p_protected` to `sparsify()`.  
  It is used with conditional sparsification to specify a proportion of the 
  transitions out of every location that won't be zeroed out.
* Added tests for `sparsify()` and related functions.
* Added `n_states` statistic to `distribution_performance()` and thus also to 
  `$metadata$sparse$stats` it reports the total number of non-zero values
  in the marginal derived distributions for all time steps in the model.  This
  is the number of states (locations in time and space) that birds can occupy
  in the model.

# BirdFlowR 0.1.0.9041
2023-12-12

* Dropped the ability to import pre-dynamic-mask models. This was announced 
  in 0.1.0.9040 but not done. 
* Fix spelling and minor documentation edits
* Fix bug in `n_parameters()` for fitted models.
* `birdflow_options()` now accepts a list with option names and their 
  new values, as well as all the previously accepted argument forms.
* Improve `validate_BirdFlowR()` error reporting.  
* Add tests for:
   *  Date lookup on preprocessed but not fitted models. Closes #147.
   * `n_paramaters()`
   * `load_model()`
   * `load_collection_index()`
   * `route_migration()`
   * `marginal_stats()`, `calc_pct_zero()`, and `sum_marginals()`
   * `find_threshold()`
   * `print()` for `BirdFlow` and `BirdFlowRoutes` objects.
   * `cache_path()`
   * `compare_geom()`
   * `birdflow_options()`
   * `build_transitions()`, `drop_transitions()`

* Expand testing for:
   * `preprocess_species()`
   * `validate_BirdFlow()`
   
# BirdFlowR 0.1.0.9040
2023-12-11

## Support for **ebirdst** 3.2022.0 added.
**BirdFlowR** can now fit models based on eBird 2022 data or 2021 data and
will preprocess using whichever version of **ebirdst** is loaded.
Both types of fitted models can be used with BirdFlowR.
Most testing will run with either version of **ebirdst** installed but CRAN
checks will only pass with the new version due to references to objects that
don't exist in the old version.

## Breaking
*  `get_dates()` 
    * Models fit using **ebirdst** 2.2021  have `$dates` columns:
    `"interval"`, `"date"`, `"midpoint"`, `"start"`, `"end"`, `"doy"`, and 
    `"week"`.
    * Models fit with **ebirdst** 3.2022 have `$dates` columns: 
    `"timestep"`, `"date"`, `"label"`, `"julian"`, and `"week"`.
    * Regardless of the `$dates` format in the model object `get_dates()` 
    returns the newer columns:  `"timestep"`, `"date"`, `"label"`, `"julian"`, 
    and `"week"`. Previously it returned the older columns.
    * Users should replace `bf$dates` with `get_dates()` and use new column
    names.

*  Inconsistent weeks. **eBird** changed the way dates are assigned to weeks
   in the 2022 version.  See notes in `get_dates()` for details. 
   **BirdFlowR** honors the date scheme used
   in the eBird data each model was built with and thus some dates will be 
   assigned to a different week with a 2021 model than they are with
   a 2022 model this will affect `lookup_timestep()`, 
   `lookup_timestep_sequence()`, and the many functions that rely on them.
   
*  Importing (old) BirdFlow models fit without a dynamic mask is no longer
   supported. Predicting with them is. Use BirdFlowR 0.1.0.39 if you want to 
   import an old hdf5 file which could then be saved with `saveRDS()`.
   Dynamic masks were added in 0.1.0.9001 (April 2023) so only models fit
   (but not imported) before then will be affected. 
       
## New

* `preprocess_species()` works with both **ebirdst** 3.2022.0 and
  2.2021.3. It will use whichever version is loaded and models fit with 
  either eBird version year can be used with BirdFlow. 

* New metadata items 
  * `birdflowr_preprocess_version`: the version of **BirdFlowR** used for 
    preprocessing.
  * `ebirdst_version`:  the **ebirdst** version used while preprocessing.

## Updates

* A number of internal changes were made to `preprocess_species()` 
  to work with  **ebirdst** v. 3.2022
  * `species` can be set to either `"example_date"` or `"yeseb-example"` to
  triggering using  **ebirdst** example data. **BirdFlowR** will silently
  switch between the two to accommodate **ebirdst**.
  * If **ebirdst** version >= 3.2022 the quality of the species model is 
  checked using the `<x>_season_quality` values instead of the dropped
  `<x>_range_modeled` information and an error is thrown if any value is 
  less than the new `min_season_quality` argument which defaults to `3`.
  With **ebirdst**  2.2021 quality is still checked with `<x>_range_modeled`.
  * With **ebirdst** >= 3.2022 all the new trends columns are dropped from 
  `ebirdst_runs` when creating the species data.  The data available via
  `species_info()` (and `$species`) is unchanged.
  * A new `dates` format is now used with 2022 models.
  
* Date lookup code was overhauled throughout the package. 
  * Most use of `$dates`  was dropped in favor of `get_dates()` to handle the 
  two date formats in use.
  
* `preprocess_species()` snapshot tests were updated to use eBird 2022 derived
  snapshots and are skipped if older versions of **ebirdst** are loaded, but
  most `preprocess_species()` tests are still run.
  
* Several internal functions documented in `ebirdst-compatibility` help
  insulate **BirdFlowR** from the changes in the **ebirdst** API and facilitate
  working with both versions.

# BirdFlowR 0.1.0.9039
2023-11-21

Updated tests to work with new example data and ebirdst 3.2022.0 

* Updated tests for new `amewoo` model in **BirdFlowModels** 
  (R package) v. 0.0.2.9002
* Added formal dependency on BirdFlowModels >= 0.0.2.9002.
* Updates to pass CRAN checks but not preprocess with **ebirdst** 3.2022.0 
        * Added `ebird/ebirdst` to remotes (to force installing development 
          version). Revert this after changes in **ebirdst**  
          47bbdfc87 are on CRAN
        * Add skip_if_unsupported_ebirdst_version() to preprocess species tests
          as it currently does for **ebirdst** 3.2022.0. 
        * Add copy of `ebirdst_weeks` to BirdFlowR as internal data.

# BirdFlowR 0.1.0.9038
2023-11-16

Trying to pass all CI checks while still using ebirdst 2.2021

* Added  version dependency <= 2.2021.3 for **ebirdst** (under imports)
  to avoid version 3.2022 
  until **BirdFlowR** is updated for the significant changes in **ebirdst**
* Removed call to `ebirdst::abundance_palette()` 
  and replaced it with the resulting values. 
  As it was **ebirdst** should have been in imports.
  This also dodges dealing with the function name change in v. 3.2022.0.
* Dropped code that dealt with older version of `$dates`.

# BirdFlowR 0.1.0.9037
2023-11-14

* Spelling
  * Checked spelling on package documentation and vignettes. 
  * Added `inst\WORDLIST` (extending dictionary of valid words). 
  * To check package spelling use `devtools::spell_check()`
  * Added section on spelling to  `./.github/CONTRIBUTING.html` 

* `export_tif()` renamed to `export_rasters()` and extended to export PNG as 
well as GeoTIFF files and optionally to convert small floating point numbers 
into larger integers for export to integer datatypes. Unit tests added.

# BirdFlowR 0.1.0.9036
2023-11-14

* Documentation editing.

# BirdFlowR 0.1.0.9035
2023-11-01

Existing code should not be affected in any way by this update.

* New function`export_tif()` facilitates exporting TIFF files of a BirdFlow 
model's distributions and dynamic masks. Could be used to replace 
functionality that used to be in `preprocess_species()`, my motivation for 
adding it is for use with Avian Flu modeling. 

* New function `pad_timestep()` converts a numeric timestep into a character 
padded with leading zeros if appropriate.  Given `1` it returns `"01"`. It is 
now used internally anywhere timestep or marginal names are padded. It's also
used by `export_tif()` for file naming if exporting single band TIFF files.


# BirdFlowR 0.1.0.9034
2023-10-17

* `preprocess_species` now adds a `week` column to `$dates` component of 
  BirdFlow objects. Initially this is identical to the `interval` column. 
  However, after truncation intervals will be renumbered to `1:n_active()` but
  weeks will always reference the week of the year.  Closes #132.
  
* linted package - fixed most issues flagged by `lintr::lint_package()`  

# BirdFlowR 0.1.0.9033
2023-10-10

* Minor update to `build_collection_index()` so it works regardless of whether 
  BirdFlowR is loaded with `devtools::load_all()` or `library(BirdFlowR)`.
  

# BirdFlowR 0.1.0.9032
2023-09-20

* BREAKING change to column names in data frame returned by 
`rasterize_distr(format = "dataframe")` 
  * Old `"time"` is now `"label"` and is an ordered factor it is derived from
  the column names in the distribution which are often but not always dates.
  * Old `"density"` is now `"value"`. 
  
* New functions to visualize distributions:
  * `plot_distr()` 
  * `animate_distr()` 

# BirdFlowR 0.1.0.9031
2023-09-08

* New function `as_distr()` converts raster (`terra::SpatRaster`) and point 
(`sf::sf`or data frames) data into distributions. Rasters will be projected
into cells that align with the BirdFlow object's cells and then cropped
and masked. With default settings the resulting values will be 
normalized to sum to 1 and any NA values will be replaced with 0.  Points are 
converted into a distribution for each point in which all the 
density is placed in the cell corresponding to the point. Projection of `sf`
and `SpatRasters` is done automatically. Data in dataframes is assumed to 
match the coordinate reference system (CRS) of the BirdFlow object unless
the `crs` argument is used to define the CRS. Addresses #116.

* bug fix in `xy_to_latlon()` that caused it to fail when `y` was omitted.

# BirdFlowR 0.1.0.9030
2023-07-27

## preprocess_species()

This update is a major overhaul of `preprocess_species()` with two primary 
goals.

* Adding support for preprocessing truncated models - models that cover only 
part of the year.   
* Cleaning up and simplifying the code by dropping some outdated parameters 
and breaking the function up into several functions.

### Dropped arguments **Breaking change**

 *  `tiff`  - Prior default of FALSE is now always used and TIFF export of
 abundance data is no longer an option.
 *  `treat_na_as_zero` prior default of TRUE is now always used.
 *  `dummy_dynamic_mask` prior default of FALSE is now always used.  
 
All three of these were a little anachronistic.  `TIFF` output original was 
used in place of including the abundance data in the `hdf5`.  The other two
arguments were added during updates to allow simulating the older behavior for
testing the update, and in both cases we've decided we like the newer behavior.

Although these are breaking changes in that previously supported arguments are
dropped, the new behavior mimics the old default behavior and I expect in most 
cases the values were not set differently than the default.

**Note:** Although TIFF output of abundances is no longer directly supported by
preprocess species any model's abundance can be exported as TIFF with:
```
r <- rast(bf)
terra::writeRaster(r, "abundance.tif")
```

### Refactoring 

This shouldn't have any effect on users.

`preprocess_species()` was broken into four functions.  The three new functions
are all internal helper functions.  They are:

 * `determine_resolution()` does the surprisingly tricky work of figuring
 out what resolution yields the desired number of fitted parameters in the 
 model.
 * `process_rasters()` generates the resampled and aggregated rasters used
 by the model from the eBird S&T rasters.
 * `export_birdflow()` exports a BirdFlow model as an hdf5 (or optionally .Rds)

### Truncation **New**

`preprocess_species()` gains a `...` argument that is used to determine model
truncation, and combination of parameters supported by 
`lookup_timestep_sequence()` works here (e.g. `season = prebreeding` and 
`start = 5, end = 10`).  The result is a non-cyclical preprocessed mode that 
only covers the transitions for the given time period.  Truncation does not 
affect the resolution determination - resolution is selected such that the full
model meets the desired number of parameters or GPU RAM. This means resolution 
isn't dependent on the truncation - facilitating possible later stitching back 
together several truncated models from the same species.

# BirdFlowR 0.1.0.9029
2023-07-25

Minor edits to vignettes and collection index.

# BirdFlowR 0.1.0.9028
2023-07-18

* Added BirdFlowOverview vignette from @dsheldon.
* Minor updates to BirdFlowR vignette.  

# BirdFlowR 0.1.0.9027
2023-07-17

* Added size column to collection index.
* Made model download explicitly binary (for windows).
* `load_model()` and related functions now work with or without trailing slash in 
   collection URL. 
* `build_collection_url()` re-uses metadata if the .Rds file hasn't changed
  and thus retains the same release date.

# BirdFlowR 0.1.0.9026
2023-07-14

Fixed bug in 'make_cache_readme()'

# BirdFlowR 0.1.0.9025
2023-07-11

 Added support for model collections
    
 New Functions:
 
 * `load_model()` User facing function to load  a model from the
   cache - downloading it first if necessary.
 * `build_collection_index()`  Run on a directory with model files
   to  add index files and convert the directory into a collection.
 * `cache_path()`  helper function. Returns the path to the (local)
   cache directory where model files are saved.  It is specific
   to the collection url.
 * `make_cache_readme()` helper function to make readme files in
   both the collection specific cache directory and it's parent
   BirdFlowR cache directory. It writes the date of the last
   cache update as well so is run anytime new files are saved
   to  the cache.
        
  Updated:
  
  * `birdflow_options()` has two new values:
      * `collection_url`: the base url for the collection.
      * `cache`:  the URL for the BirdFlowR cache directory which holds
        the collection specific cache directories.

# BirdFlowR 0.1.0.9024
2023-07-03

Most of this update was focused on allowing partial year models (#39).  The bulk 
of the work required for that goal is done, however, it is not  integrated with
`preprocess_species()`, so in practice it's not very useful yet. 

I've also split some internal code into additional helper functions which makes
the package code a little cleaner but doesn't affect users.

The one change that might affect users is the ability to subset a fitted 
model to a date range with `truncate_birdflow()`.

* New function `truncate_birdflow()` truncates the dates a BirdFlow model 
  covers. It works with both fitted and preprocessed models, 
  but is not yet integrated with preprocessing, so logistically currently is
  only useful for fitted models.
* New function `as_transitions()` converts an ordered sequence of timesteps
  into the names of the transitions that connect them.  Used in `predict()`, 
  `route()`,  `lookup_transitions()`, and `truncate_birdflow()`
* New public,`get_dates()` returns the `$dates` component of a BirdFlow
  object, replacing an old private function that was no longer used. Fixes #121.
* New metadata item `timestep_padding` tracks how much timesteps are padded in 
  transition and marginal names. `preprocess_species()` and `new_BirdFlow()` 
  are updated to produce it.  
* New internal function `get_timestep_padding()` retrieves the above or, for 
  older models figures it out from the model structure.
* New internal function `make_marginal_index()` derived from code previously in 
 `import_birdflow()` is used by both `import_birdflow()` and 
 `truncate_birdflow()`
* New internal function `ts_info()` determines if a timestep sequence is forward
  or backwards and whether it crosses the year boundary.  
  
# BirdFlowR 0.1.0.9023
2023-06-20

### Overview

  This update shouldn't break anything but it will change default behaviors of
  a few functions:
   1. `preprocess_species()`  Models preprocessed after this update will 
  be different then older models. 
  
   2. `lookup_timestep_sequence()` now allows looking up a cyclical
   sequence that represents a full year of transitions and `season = all` 
   returns the one more timestep then it used to `c(1:52, 1)`). 
   
   3. The code in `distribution_performance()` is unchanged but its default 
   behavior is affected by the change in `lookup_timestep_sequence()`. It now
   includes the transition from the last to first timesteps in the calculations 
   with the default time arguments.
   
### Details

* Fixed bug in `preprocess_species()` that caused it to sometimes create
  BirdFlow models where the pixels didn't align with the origin.  There's 
  nothing wrong with the previous behavior from a modeling standpoint
  but always aligning to the origin has some logistical benefits and was the
  original intent. 

* Fixed bug in `reformat_distribution_labels()` with 3 dimensional 
  input it would erroneously change the second dimension name to `"time"`.
  This caused `predict()` to produce confusing dimension labels when called on 
  multiple distributions at once.

* `lookup_timestep_sequence()` now allows full cycle sequence and returns 
  full cycle sequences with `season = "all"`.  This means including all 
  timesteps and then repeating the first timestep.  This also allows setting
  `start`  and `end` to the same timestep as well (to loop through a whole year).
  This will change results from `distr_performance()` for whole year models.
  
* `preprocess_species()` gains a new argument and default behavior.  It now 
   replaces NA values in the distributions with zero prior to transforming 
   and changing the resolution, and uses bilinear interpolation in place of 
   nearest neighbor.  Set `treat_na_as_zero` to `FALSE` for old behavior. 
   The new way seems better though.  See 
   detailed explanation in 
   [Issue #12](https://github.com/birdflow-science/BirdFlowR/issues/12).
  

# BirdFlowR 0.1.0.9022
2023-06-15

### Overview

This update merges `route_migration()` into `route()` which can now do 
everything either of these functions previously did and at least one thing they
couldn't ([#88](https://github.com/birdflow-science/BirdFlowR/issues/88)). 

The returned object from `route()` is also changed: `$lines` are 
dropped and `$points` are now returned in a new S3 class `BirdFlowRoutes`. With
`plot_routes()` I don't think returning the lines is as useful as it was.  The 
lines can be recreated with `sf:st_as_sf(rts)`.

This closes [#103](https://github.com/birdflow-science/BirdFlowR/issues/103) and
[#88](https://github.com/birdflow-science/BirdFlowR/issues/88)

Details follow.

### BREAKING CHANGES to `route()`
- Dropped `row` and `col` arguments. `x_coord` and  `y_coord` are sufficient 
  and I think preferred by users.
- Renamed `n_each` argument to `n`. 
- Renamed `x` argument to `bf`.
- Changed order of arguments.
- Added `from_marginals = TRUE` argument that controls which distributions are
  used when sampling starting locations; I expect most users not to use this 
  argument.
- Specifying starting locations (via `x_coord` and `y_coord`) is now optional.
  If those arguments are NULL (the default) then `n` starting positions will be 
  sampled from the species distribution for the starting timestep. This 
  sampling  was previously done only in `route_migration()`.
- Changes to the returned object for both `route()` and `route_migration()`
    * `route()` now returns a`BirdFlowRoutes` object, which is an extension of 
      a data frame and almost always behaves like a data frame. 
    * The `$lines` component of the previously returned list has been dropped,
      and a modified version of what was `$points`  is returned. Use the
      new `st_as_sf()` method for the`BirdFlowRoutes` object to produce the 
      **sf** lines.
    * The old `points$route` column has been renamed`route_id` 
    * **Experimental** the returned `BirdFlowRoutes` object has attributes that 
      contain much of the ancillary data in the parent BirdFlow object:  `geom`, 
      `species`, `metadata`, and `dates`.  Additionally, a new item is added to 
      `metadata`: `route_type = "synthetic"`.  This may be useful if we also
      create `BirdFlowRoute` objects from tracking data. If you want to 
      insulate yourself from the experimental aspects of this call 
      `as.data.frame()` on the returned routes.
    
### Non-breaking changes
  * Edits to vignettes and readme.
  * `route_migration()` is deprecated. Its arguments are unchanged and it will
    work but now throws a warning. Please transition to using `route()` with
    the `season` argument. The returned object is changed - see changes to
    `route()` above.
  * A new method for `st_as_sf()` convert `BirdFlowRoutes` objects to **sf**
    objects with either points or lines.  Use `st_as_sf(rts)` for lines or
   `st_as_sf(rts, type = "point")` to convert to an *sf* points object.
  * New `plot()` method for `BirdFlowRoutes` objects dispatches to 
   `plot_routes()`.
  * **Experimental:** `plot_route()` and `plot(BirdFlowRoutes)` now do not 
    require the BirdFlow object - as long as the attributes added by `route()` 
    are present.  If the `bf` argument is used it supersedes the route 
    attributes so including it will make your code less likely to break if we 
    change the `BirdFlowRoutes` class.
    This is kind of nice though:
      `route(bf, 5, season = "prebreeding") |> plot()` 
  * A new print method for `BirdFlowRoutes` is there mostly to hide the
    attributes while printing - but also has a new header line that states what
    the species and object is.
  * `plot_routes()` gains additional arguments to control line widths and dot 
    sizing.  `animate_routes()` has access to them via `...`. 
    
### A note about the new `BirdFlowRoutes` class. 
This class is very much a work in progress.  Here's where we stand.

 * I'm committed to `route()` returning a data frame like object that contains
   the data formerly in`rts$points` and I think the columns are fairly stable at 
  this point.
 * I'm fairly committed to making it an S3 "BirdFlowRoutes" object and, at a
   minimum, including the CRS somewhere in the attributes. 
 * I'm less committed to including all the other additional attributes. @slager
   is working on making BirdFlowRoutes like objects 
   from tracking data and I hope that together we can figure out what data this 
   object should look like. 

Right now the extra information in this class is used in only three places:

  * `plot_routes()` and related `plot(BirdFlowRoutes)` 
  * `st_as_sf(BirdFlowRoutes)`
  * `print(BirdFlowRoutes)`
  
In all other ways it should behave like a data frame. Some data frame 
manipulations will preserve attributes and class. Many will result in a standard
data.frame.



# BirdFlowR 0.1.0.9021

* Added logo

# BirdFlowR 0.1.0.9020

This update fixes bugs and cleans things up.  It shouldn't break existing code.

* Fixed bug in `distribution_performance()` that caused an error with
`metrics = "md_traverse_cor"` and added tests.

* Removed `tests/testthat/Rplots.pdf` from version control and added it
to `.gitignore`.

* Added **gifski** to imports, and moved **rnaturalearthdata** from suggests to 
imports. This should make installation smoother and prevent gganimate::animate
from writing lots of image files in the working directory.  Closes #102.

* `interval_log_likelihood()` now returns a zero row data.frame if `intervals` 
or `observations` have zero rows. Care is taken to return the same column names
and data types as would be returned with data.  Previously zero row input 
resulted in an unhelpful error. Closes #95.

# BirdFlowR 0.1.0.9019
2023-06-07

### BREAKING CHANGES to timestep sequence arguments 
 
  * `lookup_timestep_sequence()` gains new `season` parameter as 
  its second argument. Previously season could be specified via `start` but it 
  was confusing, especially when called via `...` from other functions.  
  * `lookup_timestep_sequence()` `n` argument renamed to `n_steps`.
  * All functions that allow specifying a sequence of timesteps (or date range)
  now use `...` passed to `lookup_timestep_sequence()` to do so.  Previously 
  for `predict()` and `route()` some of these were explicit arguments that
  were then passed individually to `lookup_timestep_sequence()`
  
  **Affected functions**
  
  This impacts calls to functions that pass arguments on to 
  `lookup_timestep_sequence()`:

  * `lookup_timestep_sequence()` (directly)
  * `lookup_transitions()` 
  * `predict()`
  * `route()`
  * `distribution_performance()`
  * `animate_movement_vectors()`
  
  **Broken usage**
  
  *  Code that passed season names explicitly to `start` will be broken: 
  `lookup_timestep_sequence(bf, start = "prebreeding"`) .
  * Code that relied on the position of `start` and `end` to pass dates or
  timesteps will also fail: `lookup_timestep_sequence(bf, 1, 5)`.
  * Use of the `n` argument, will need to be updated to `n_steps`
  
  **Unaffected usage**
  
  * Passing dates or timesteps to `start` and `end` by name should be
  unaffected: `lookup_timestep_sequence(bf, start = 1, end = 5)`.
  * Passing a season by position works both before and after changes:
  `lookup_timestep_sequence(bf, "prebreeding")`
  
  As a general rule going forward use argument names beyond the first two 
  arguments (`x` and `season`).
  
  **Default values** 
  
  * Default values in `lookup_timestep_sequence()` were switched from missing to
  NULL - similar to changes made to `preprocess_species()` in 
  [BirdFlowR 0.1.0.9009]. This shouldn't affect most users but will make 
  setting arguments programmatically slightly easier.
  
  * If no arguments other than the BirdFlow object are passed to 
  `lookup_timestep_sequence()` it will return all timesteps.  This is consistent
  with the change in [BirdFlowR 0.1.0.9017]. This default is now inherited by
  all the affected functions listed above. For some of the affected functions
  this is a change from the prior behavior of throwing an error if the time
  sequence was not specified. This is unlikely to affect existing code.

# BirdFlowR 0.1.0.9018
2023-06-06  

* `sample_distr()` has a new argument `format` that allows specifying 
 `distr`, `xy`, `latlon`, or `i` (location index) as the output format.  
 The default is `distr`  which mimics previous behavior. This is in response to 
  [a comment](https://github.com/birdflow-science/BirdFlowR/issues/88#issuecomment-1544401203)
  in issue [#88](https://github.com/birdflow-science/BirdFlowR/issues/88),
  the `"xy"` return format greatly simplifies calling `route()` with a sample
  from a distribution.
* BirdFlow vignette updated to use `sample_distr(format = "xy")` and to use
  `plot_routes()`.
* Bug fixed in `xy_to_latlong()` where column order in returned object was 
  backwards.
* Index conversion functions that previously returned a two column matrix now
  return a two column data.frame: `xy_to_latlon()`, `latlon_to_xy()`,
  `i_to_rc()`, and `i_to_xy()`.  I found that most often I wanted to refer to 
  columns by name making the data.frame format easier.

# BirdFlowR 0.1.0.9017
2023-06-05

### Revamping performance metrics

* DEPRECATED `evaluate_performance()` is now deprecated. Use  `distribution_performance()` instead.
* NEW `distribution_performance()` replaces `evaluate_performance()` and has several differences.
  * `traverse_cor`is replaced by two very similar metrics `st_traverse_cor` and `md_traverse_cor`. The first is a bug free version of the old `traverse_cor()`. The second is identical except it makes the predictions starting with a marginal rather than a status and trends distribution. 
  * `*_traverse_cor` metrics now utilize the dynamic mask to filter which cells are used in the correlation calculation.
  * `...` arguments are passed to `lookup_timestep_sequence()` to allow
  calculating metrics for just part of the year.  
 
* `sparsify()` (which still isn't updated for dynamic masking) now uses `distribution_performance()` instead of `evaluate_performance()`.
* `lookup_transitions()` (private function) now has only one formal argument (x) all other arguments are now passed to `lookup_timestep_sequence()` via `...` those other arguments are unchanged though so calling it shouldn't change.
* `lookup_timestep_sequence()` now has a default value `start = "all"` 
previously there was no default. This is mostly to make use in other functions via `...` easier as `all` is the most common default in calling functions.



# BirdFlowR 0.1.0.9016
2023-06-03

* Edited Contributing guide
* `preprocess_species()` now throws better errors when there are problems
   with the species argument (NA, NULL, or unresolvable species names).
* Additional tests for`preprocess_species()` for error conditions and to test
  file output.

# BirdFlowR 0.1.0.9015
2023-06-02
* Added tests for `add_dynamic_mask()`  #101
* Added RMarkdown README.  #104
  * Now the README.md file is generated from the README.Rmd file (edit the .Rmd)
  * Added example plots to README.
* Added contributing guide.  #97

# BirdFlowR 0.1.0.9014
2023-05-25

* Updated `plot_routes()`
  * Works over year boundary
  * Color bar only shows the used date range.
  * Additional arguments to control appearance:    
     `use_seasonal_colors`, `pal`, `barheight`
  
* Updated animate_routes() added ... argument that is passed onto
  `plot_routes()` to control of appearance. 

# BirdFlowR 0.1.0.9013
2023-05-22

* New `animate_routes()` #93
* Updated `plot_routes()` 
  * Added dot for stay = 1 so all segment end points are visibly marked.
  * Subtle changes to code to support animation.
* Updated documentation of [route()] and [route_migration()] to full describe
  additional columns added in 0.1.0.9012

# BirdFlowR 0.1.0.9012
2023-05-17

* Fixed bug in `route()` and `route_migration()` that prevented routing over 
  year boundary.
* New: 
  - `plot_routes()` for plotting routes with ggplot2.
  - `compareGeom()` methods for BirdFlow objects, possibly mixed with
    `terra::SpatRaster` objects. See `terra::compareGeom()`
  - `proportion_of_year()` (internal function) converts a date into a number
    between 0 and 1.
* Updated:
  - `flatten_raster()` supports multiple input formats  #23
  - `rasterize_distr()` supports multiple output formats #23
  - `route()` and `route_migration()` output return additional columns in the 
    `points` item.  
* Deprecated:
  - `expand_distr()` is now an internal function.  Users can use
  `rasterize_distr( format = "numeric")` instead.

# BirdFlowR 0.1.0.9011
2023-05-11

* Minor change to `interval_log_likelihood()` for clearer code and to avoid 
warning when tibbles are used. 


# BirdFlowR 0.1.0.9010
2023-05-09

* Dockerfile: Bump rocker image to R 4.3.0 and error out of build on package installation failure

# BirdFlowR 0.1.0.9009
2023-05-09

* `preprocess_species()` now defaults the `res` parameter to NULL, making it 
easier to script around `preprocess_species()`

* Tweaked some tests to better run across different development environments

# BirdFlowR 0.1.0.9008
2023-05-09

* `import_birdflow()` now converts logical hyperparameters masquerading as 
  factors into logical.  Addresses #81.

* Dropped legacy import functions:
  - `import_birdflow_v1()` for models that predate the R Package, this import
  required first converting a pickle file to an hdf5 and then importing from 
  that and a TIFF file with the distributions.  
  - `import_prototype()` for the models that were includes in the prototype 
  shiny app.

# BirdFlowR 0.1.0.9007
Fixed URL.

# BirdFlowR 0.1.0.9006
Cleaned up bad formatting in _pkgdown.yml. 

# BirdFlowR 0.1.0.9005
Added argument `n` to `lookup_timestep_sequence()`, an alternative to `end`, 
`n` indicates how many transitions should be in the resulting sequence. 
`route()`, `lookup_transitions()`, `predict()`, and `animate_movement_vectors()`
all gained either an explicit `n` parameter or access to it via `...`.


**Breaking change:** The pre-existing `n` parameter to `route()` was renamed 
`n_each` to avoid conflict with the new `n` parameter which is passed on to
`lookup_timestep_sequence()`. 

Closes #76.

# BirdFlowR 0.1.0.9004
Made changes to support pkgdown.

* Added links to .yml
* Added "BugReports" field to DESCRIPTION file
* Added a second link to URL field in DESCRIPTION file linking to github repo
* Switched development mode in .yml to `unreleased`.  `auto` wasn't working with
  version 0.0.1.x.  When we have our first formal release it should
  be switched back to `auto`.

# BirdFlowR 0.1.0.9003

* `evaluate_performance()` is now exported.
* `evaluate_performance()` now uses the dynamic mask when calculating 
correlations (excluding the masked out cells). This means a bunch of cells 
that were essentially forced to zero in both sides of the correlation aren't 
included in the calculation. Correlations will get lower and the effect will 
be more pronounced with lower correlations.


# BirdFlowR 0.1.0.9002

### Added movement vector visualizations

These functions calculate and visualize the average movement out of each cell 
in a BirdFlow model during a particular transition.

New Functions

 * `calc_movement_vectors()`  returns a data frame with information on the 
   average movement out of each cell in the model for a given timestep.
 * `plot_movement_vectors()`  plots movement vectors directly from a BirdFlow 
   model for a given timestep. Returns a `ggplot` object.
 * `animate_movement_vectors()` creates an animation of the vectors over a 
   series of timesteps.  Returns a `gganim` object.

# BirdFlowR 0.1.0.9001

## Switch to Dynamic masking

### Summary 

Dynamic masking is a major overhaul of the package in which the cells the model
acts on will change over time and be limited to cells with non-zero value 
in the eBird S&T distributions. This means that the marginal dimensions will
vary over time, and they will often not be square.

The intent is to isolate the user and from the changes. In 
particular the output and input distribution objects will still contain all
the active cells and most public function arguments are unchanged. 

Functions that interact with the marginals now have to convert internally 
between standard distributions covering all active cells, and the the 
dynamically masked distributions that conform to the marginal dimensions. 
`predict()`, `route()`, and `import_birdflow()` all had substantial updates.

BirdFlow objects gained a great circle distance and a dynamic mask, both created
in preprocessing, and variable marginals and transitions dimensions.  The great
circle distance is used by python and then dropped (in python) from the fitted
model, as it's fairly large and easy to recalculate with 
`great_circle_distances()`. 


### Detailed changes

 * **BirdFlow object format changes**
    - New `/distances/` contains values from a distance matrix of
      great circle distances in km between each pair of locations - converted 
      to a vector of non duplicated values by `shorten_distance_matrix()` full 
      matrix can be recreated with `expand_distance_matrix()` this is added to 
      the export HDF5 by `preprocess_species()` but not retained in the fitted
      models, the distances can be recreated with `great_circle_distances()`.
    - New `/geom/dynamic_mask` this is a matrix with a row for each active cell 
      in the model (see `n_active()`) and a column for each timestep.  The cells
      have a one to one relationship with the cells in `/distr' and are TRUE if
      the cell is included in the model for the timestep and FALSE otherwise.
    - `/metadata/birdflow_version` is now 3.  
    - New `/metdata/birdflowr_version` stores the package version when 
      `preprocess_species()` is/was called.
    - New `metadata/hyperparameters` and `metadata/loss_values` contain
      information generated during python model fitting.  
    
 * **New functions**
    - `get_dynamic_mask()` similar to `get_distr()` but for dynamic mask data.
    - `add_dynamic_mask()` updates an old BirdFlow object (in an R session) by 
       adding a dynamic mask.  This is mainly to facilitate the transition to 
       allow testing the package with the old models.
    - `has_dynamic_mask()` returns a logical.
    - `import_birdflow_v3()` internal function is called by `import_birdflow()` 
       for version 3 BirdFlow HDF5 files.
    - `great_circle_distances()` creates a great circle distance matrix encoding
       the distance (km) between every pair of cells in a BirdFlow object.
   
 * **Updated functions**
    - `import_birdflow()` 
        - now works with version 2 and 3 BirdFlow HDF5 files 
          (added version 3, dropped version 1)
        - reads the dynamic mask
        - reads metadata/hyperparameters
        - read metadata/loss_values
    - `get_distr()` with `from_marginals = TRUE` now has to expand the truncated 
       distribution calculated from the marginal out to a full distribution.
    - `predict()` and `route()` have to map from full distributions, to 
       truncated distributions before applying the transition and then, expand
       back to full for output.
    -  `validate_birdflow()` now checks the dimensions of the marginals and 
       transition matrices against the dynamic mask cell counts for each 
       timestep.
    -  `preprocess_species()` 
       - now calls `great_circle_distances()` and `shorten_distance_matrix()`to 
         create "/distances" component of the HDF5
       - adds "geom/dynamic_mask" which is a logical matrix indicating 
         which cells of "/distr" are non-zero for each timestep.
       - Has updated heuristic to set resolution based on the number of
         parameters given dynamic masking.
       - new argument `dummy_dynamic_mask()` adds a dynamic mask that is all
         TRUE to force fitting of the prior style of BirdFlow object.
   - `sparsify()` has not been updated to work with dynamic masks, but will now
      throw an error telling you that if you try to use it on a BirdFlow object
      with a dynamic mask.

# BirdFlowR 0.0.0.9075
2023-04-18  

Added BirdFlow methods for generics defined in **sf**
* `st_crs()`  returns an object of class `crs` used by **sf** to define 
  coordinate reference systems (CRS).
* `st_bbox()` returns an object of class `bbox` containing both the extent and
  CRS of a BirdFlow object.  Can be passed to `st_as_sfc()` to convert to a 
  polygon.


# BirdFlowR 0.0.0.9074
2023-04-13

* Changed behavior
  - `get_naturalearth()` and related functions now by default crop off the
    buffer after transforming the Natural Earth data so that the returned 
    object extent matches the extent of `x`. Whether this cropping occurs is 
    now controlled by `keep_buffer` which defaults to FALSE.  Previously it was    
    controlled by `match_extent`.

# BirdFlowR 0.0.0.9073
2023-04-11

* Changed behavior:  
  - `lookup_timestep()` and timestep lookup throughout the package is now 
  consistent with `ebirdst::date_to_st_week()` this wasn't previously true. Some
  dates near the edges of the week breaks will end up classified into a 
  different timestep than previously.

  - `preprocess_species()` now saves the breakpoints from 
  `ebirdst::ebirdst_weeks` as `start` and `end` instead of `week_start` and 
  `week_end`.

* New functions: 
  - `lookup_timestep_sequence()`  workhorse function for processing date
    range input to other functions. Generate forward or backward sequences from
    timesteps, dates, or season name input, possibly with a season buffer.

  - `lookup_season_timesteps()` narrowly focused helper, returns forward 
    timestep sequences associated with a season possibly with a buffer 
    (in timesteps) beyond the edge of the season.
  
* Updated functions to use `lookup_timestep_sequence()`
  - `lookup_transitions()`
  - `route()`
  - `route_migration()` 
  - `predict()`
* Updated functions to use `lookup_timestep()`
  - `get_distr()`

* Addresses issues: 
  - Fixes bug in #66 where date lookup forward across the year boundary failed.
  - Addresses #68 by providing a function to lookup timestep series based on 
    season names (and adds a buffer ability).
  - Closes #56 time now is processed mostly by `lookup_timestep()` for points in
    time, and `lookup_timestep_sequence()` for date ranges. Point lookup now
    uses `findInterval` on breaks derived from `ebirdst::ebirdst_weeks` rather
    than from `which.min()` on a difference from center of the nominal day for 
    each week. This should also make time lookup compatible with partial year
    BirdFlow models.

# BirdFlowR 0.0.0.9072
2023-04-11

* Fixed bug introduced when **ebirdst** 2.2021.1 converted all columns of
 `ebirdst_runs` to character. `preprocess_species()` now defensively forces the 
  columns that should be (and were) logical to logical, and numeric to numeric. 


# BirdFlowR 0.0.0.9071
2023-04-06

* Added functions `latlon_to_xy()` and `xy_to_latlon()` to convert from 
WGS84 (EPSG:4326) and x and y coordinates in the BirdFlow objects CRS.  Fixes 
#64.  

* CHANGED behavior in a bunch of the index conversion functions, previously, 
many threw errors with NA input or values out of range. Most of them now return 
NA in both of those situations.  This made sense to me in the context of #61.

# BirdFlowR 0.0.0.9070
2023-04-06

* Switching development version number scheme.  From now on I will increment
the development version by one with every change in the 
main branch (merged pull request). Previously the version was the issue number.  

* Fixed #61 (and added test). Now `interval_log_likelihood()` sets `exclude` and 
`not_active` columns to TRUE if either of the involved observations are entirely
outside of the extent of the BirdFlow object. Previously locations outside of
extent resulted in an error.

# BirdFlowR 0.0.0.9054
2023-03-30

* New interval_loglikelihood() calculates log likelihood for banding and 
tracking data given a BirdFlow model. 

* new supporting functions:
 - `drop_transitions()` 
 - `is_location_valid()`, `is_distr_valid()` test whether locations and 
 distributions are valid inputs for `predict()` and `route()`
 - `lookup_timestep()` 


# BirdFlowR 0.0.0.9022 
2023-03-27

* `get_naturalearth()` and related functions now throw a helpful warning if
the extent is empty, and, as before, return an empty `sf` object.

* `get_naturalearth()` has an improved, more robust way of processing the vector
data that works for specific projections. Currently it is used only for 
Mollweide ("moll") and Lambert Equal Area ("laea") based CRSs.

* `get_naturalearth()` has a new argument `match_extent` which if set to `TRUE`
causes the result to be clipped to the precise extent of the input object (`x`).
Use `TRUE` when plotting with ggplot2 so that the Natural Earth data doesn't 
result in an expanded plot extent. The default, `FALSE` is appropriate for base 
R plotting so that the Natural Earth Data runs up to and beyond the edge of the
plot regardless of whether the aspect ratio of `x` matches the plot window's.

* `get_naturalearth()` has a new argument `use_old_method` which, if TRUE 
forces the less robust method regardless of projection. It is there only for 
testing the  function and should be left at the default for all other uses.

# BirdFlowR 0.0.0.9044

* new function `build_transitions(x)` will populate `x$transitions` with both
forward and backwards transitions. This is recommended before repeated 
`forecast()` or predict calls as it avoids repeatedly making transitions on the 
fly.  It does triple the memory usage for the BirdFlow object so should probably 
only be done with sparse BirdFlow models. It currently will work on any but 
throws a warning if the BirdFlow model is not sparse. 

* previously `transitions_from_marginal()` produced a standard matrix. Now, if 
the marginal is sparse the conversion uses and preserves the sparse 
matrix.  Similarly, if the transition matrix is sparse `predict()` and `route()`
both calculate on sparse objects.  This should speed those two functions up
significantly.

# BirdFlowR 0.0.0.9036

* replaced `forecast()` with a BirdFlow method for `predict()`.  
  
# BirdFlowR 0.0.0.9033

The end result of all the time stuff enumerated below is that (1) you can set 
the time format, (2) as before when possible the time associated with each 
distribution is stored in dim names, (3) when it's a single distribution and 
thus not possible to store the time in the dim names a new `"time"` attribute 
is added to the object to store the time associated with the distribution.

* new `birdflow_options()` allows setting global options that persist for the 
  session.  Currently there are two options `verbose` and `time_format`.
 
* The `time_format` option controls the labeling of distributions either 
  in column names if there are more than 1 distribution or via the "time" 
  attribute (See below) if there's a single distribution. This affects 
  `get_distr()`, `predict()`, and `rast()`.

* `get_distr()` now adds an attribute `"time"` when returning a single 
  distribution as a vector.  Multiple distributions already used matrix column 
  names to record the time associated with each distribution.

* Similarly, `expand_distr()` when called on a single distribution will preserve
  the `"time"` attribute in its output matrix, and 
  `flatten_raster()` called on a single distribution's raster (a matrix) will 
  also preserve the "time" attribute in its output vector.

* `rasterize_distr()` and thus also `rast()` now convert the `"time"` attribute
  to a layer name in the output raster when there is a single distribution. They 
  previously and still do this with the distribution column names for multiple
  distributions.
  
* The names for the column dimension of distribution matrices has changed from
  "timestep" to "time" both in the internal, stored object and in returned 
  objects from `get_distr()` and `forecast()`.
  
* `fix_dead_ends()`, `preprocess_species()`, and `sparsify()` now honor
`birdflow_options("verbose")` - printing progress and information only if 
it is `TRUE`. 
 
# BirdFlowR 0.0.0.9034

* Changed `preprocess_species()` `gb` argument to `gpu_ram`.  

# BirdFlowR 0.0.0.9035

* `sparsify()` no longer throws confusing warning (#35)
* First argument of `sparsify()` changed from `bf` to `x` (consistent with #19) 

# BirdFlowR 0.0.0.9038

* Updated docker file to work with Apple silicon and not to use cached github 
  repos. (commit #37)

* Updated installation instructions for Docker (commit #37)

* Route output snapshot testing no longer depends on the print(lines) but does
  check novel points in the points component of the return value.  This should 
  fix an issue currently with it failing CRAN check because sf:: changed 
  slightly how it prints a summary of an SF object to screen and also make the
  snapshot test a little less hair trigger.


# BirdFlowR 0.0.0.9028
2023-03-08

* New `vignette("Preprocess")` covers preprocessing, importing, and  
  sparsification 

* New `vignette("Installation")` covers R, RStudio, and package installation; 
  and installing in a  Docker container. 

* Updated installation instructions everywhere to use remotes instead of 
  devtools and do not re-install packages unnecessarily.

* Expanded "Learn More"" section in README.md to link to all vignettes as well
  as the BirdFlow paper.

# BirdFlowR 0.0.0.9021
2023-03-03

* Updated preprocess_species()
  - Now only downloads the files it needs [#21](https://github.com/birdflow-science/BirdFlowR/issues/21)
  - Slight adjustment to resolution heuristic, it should take fewer steps to 
  find the right resolution.
  - Added  additional tests to cover error conditions and clipping

# BirdFlowR 0.0.0.9019
2023-03-01

* Nomenclature cleanup (round 1)
  - collapse_distr(x, bf) -> collapse_raster(raster, bf)
  - evaluate_performance(bf) -> evaluate_performance(x) 
  - expand_distr(x, bf) -> expand_distr(distr, bf)
  - get_distr(which, bf, from_marginals) -> get_distr(x, which, from_marginals) 
  - get_transition(x, bf) -> get_transition(x, transition) 
  - lookup_transitions(start, end, bf, direction) -> lookup_transition(x, start,
    end, direction) 
  - sample_distr(x, bf) -> sample_distr(distr, bf)

* No longer exported:
  - evaluate_performance()
  - find_dead_ends()
  - find_threshold() 
  - fix_dead_ends()
  - import_prototype() 
  - lookup_transitions() 
  - new_BirdFlow()
  - transition_from_marginal()

* Added "@keyword internal" to documentation for all non-exported functions.  
  This removes the documentation from the package manual and index, but it's 
  still accessible with ?function_name. 


# BirdFlowR 0.0.0.9016
2023-02-27

* Added package down. Starting to use [semantic versioning](https://semver.org/).

# BirdFlowR 0.0.0.9017
2023-02-27

* Fix bug introduced by **ebirdst** 2.2021.0 (switch from **raster** to 
**terra**) [#17](https://github.com/birdflow-science/BirdFlowR/issues/17).

# BirdFlowR 0.0.0.9003
2023-02-27

* Updated installation instructions.  Closing
  [#11](https://github.com/birdflow-science/BirdFlowR/issues/11).

* Added docker file. 
  [Usage instructions.](https://github.com/birdflow-science/BirdFlowR/pull/15#issuecomment-1445152787) 

# BirdFlowR 0.0.0.9002
2023-02-23

* Added "biocViews:" before "Imports:" in DESCRIPTION so that devtools can find
and install rhdf5 (from Bioconductor) while installing the packages BirdFlowR 
needs. [#13](https://github.com/birdflow-science/BirdFlowR/issues/13) 

* Vignette now attempts to load rnaturalearthdata with utils::install.packages() 
  instead of `devtools::install_cran()` in attempt to fix
  [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 

* Updated `get_naturalearth()` so that it handles some cases in which it 
  previously failed.  In particular extents that span more than 180 deg of 
  longitude, and extents that cross the 180 deg. meridian that defines the edge 
  of the WGS84 projection both now work. The function still doesn't handle 
  polar projections or global extents in most projections. Fixes 
  [#14](https://github.com/birdflow-science/BirdFlowR/issues/14) 

# BirdFlowR 0.0.0.9001
2023-02-21

* Switched order of package installation in readme and added installation of
  rnaturalearthdata to vignette
  [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 

* Added a `NEWS.md` file to track changes to the package.
