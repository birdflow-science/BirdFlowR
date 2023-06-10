# BirdFlowR 0.1.0.9021

* Added logo

# BirdFlowR 0.1.0.9020

This update fixes bugs and cleans things up.  It shouldn't break existing code.

* Fixed bug in `distribution_performance()` that caused an error with
`metrics = "md_traverse_cor"` and added tests.

* Removed tests/testthat/Rplots.pdf from version control and added it
to .gitignore.

* Added **gifski** to imports, and moved **rnaturalearthdata** from suggests to 
imports. This should make installation smoother and prevent gganimate::animate
from writing lots of image files in the working directory.  Closes #102.

* `interval_log_likelihood()` now returns a zero row data.frame if `intervals` 
or `observations` have zero rows. Care is taken to return the same column names
and data types as would be returned with data.  Previously zero row input 
resulted in an unhelpful error. Closes #95.

# BirdFlowR 0.1.0.9019
2023-06-07

### BREAKING CHANGES to timstep sequence arguments 
 
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
  setting arguments programatically slightly easier.
  
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
* NEW `distribution_perfomance()` replaces `evaluate_performance()` and has several differences.
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
     use_seasonal_colors, pal, barheight
  
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
  that and a .tif file with the distributions.  
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
* Added BugReports field to DESCRIPTION file
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

### Added movment vector visualizations

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
between standard distributions covering all active cells, and the the dynamicly
masked distributions that conform to the marginal dimensions. `predict()`, 
`route()`, and `import_birdflow()` all had substantial updates.

BirdFlow objects gained a great circle distance and a dynamic mask, both created
in preprocessing, and variable marginals and transitions dimensions.  The great
circle distance is used by python and then dropped (in python) from the fitted
model, as it's fairly large and easy to recalculate with 
`great_circle_distances()`. 


### Detailed changes

 * **BirdFlow object format changes**
    - New `/distances/` constains values from a distance matrix of
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
         TRUE to force fitting of the prior style of birdflow object.
   - `sparsify()` has not been updated to work with dynamic masks, but will now
      throw an error telling you that if you try to use it on a BirdFlow object
      with a dynamic mask.

# BirdFlowR 0.0.0.9075
2023-04-18  

Added BirdFlow methods for generics defined in **sf**
* `st_crs()`  returns an object of class `crs` used by **sf** to define 
  coordinate reference sytems (CRS).
* `st_bbox()` returns an object of class `bbox` containing both the extent and
  crs of a BirdFlow object.  Can be passed to `st_as_sfc()` to convert to a 
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

* Fixed bug introduced when ebirdst 2.2021.1 converted all coljumns of
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
Mollweide ("moll") and Lambert equal area ("laea") based CRSs.

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
the mariginal is a sparse the conversion is done on and preserves the sparse 
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

# BirdFlowR 0.0.0.9021  2023-03-03

* Updated preprocess_species()
  - Now only downloads the files it needs [#21](https://github.com/birdflow-science/BirdFlowR/issues/21)
  - Slight adjustment to resolution heuristic, it should take fewer steps to 
  find the right resolution.
  - Added  additional tests to cover error conditions and clipping

# BirdFlowR 0.0.0.9019  2023-03-01

* Nomenclature cleanup (round 1)
  - collapse_distr(x, bf) -> collapse_raster(raster, bf)
  - evaluate_perfomance(bf) -> evaluate_performance(x) 
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


# BirdFlowR 0.0.0.9016  2023-02-27

* Added package down. Starting to use [semantic versioning](https://semver.org/).

# BirdFlowR 0.0.0.9017  2023-02-27

* Fix bug introduced by ebirdst 2.2021.0 (switch from raster to terra)
[#17](https://github.com/birdflow-science/BirdFlowR/issues/17).

# BirdFlowR 0.0.0.9003  2023-02-27

* Updated installation instructions.  Closing
  [#11](https://github.com/birdflow-science/BirdFlowR/issues/11).

* Added docker file. 
  [Usage instructions.](https://github.com/birdflow-science/BirdFlowR/pull/15#issuecomment-1445152787) 

# BirdFlowR 0.0.0.9002  2023-02-23

* Added "biocViews:" before "Imports:" in DESCRIPTION so that devtools can find
and install rhdf5 (from Bioconductor) while installing the packages BirdFlowR 
needs. [#13](https://github.com/birdflow-science/BirdFlowR/issues/13) 

* Vignette now attempts to load rnaturalearthdata with utils::install.packages() 
  instead of devtools::install_cran() in attempt to fix
  [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 

* Updated get_naturalearth() so that it handles some cases in which it 
  previously failed.  In particular extents that span more than 180 deg of 
  longitude, and extents that cross the 180 deg. meridian that defines the edge 
  of the WGS84 projection both now work. The function still doesn't handle 
  polar projections or global extents in most projections. Fixes 
  [#14](https://github.com/birdflow-science/BirdFlowR/issues/14) 

# BirdFlowR 0.0.0.9001   2023-02-21
* Switched order of package installation in readme and added installation of
  rnaturalearthdata to vignette
  [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 

* Added a `NEWS.md` file to track changes to the package.
