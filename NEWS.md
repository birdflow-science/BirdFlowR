# BirdFlowR 0.0.0.9044

* new function `build_transitions(x)` will populate `x$transitions` with both
forward and backwards transitions. This is recommended before repeated 
`forecast()` or predict calls as it avoids repeatedly making transitions on the 
fly.  It does triple the memory usage for the BirdFlow object so should probably 
only be done with sparse BirdFlow models. It currently will work on any but throws a warning if the BirdFlow model is not sparse. 

* previously `transitions_from_marginal()` produced a standard matrix. Now, if 
the mariginal is a sparse the conversion is done on and preserves the sparse 
matrix.  Similarly, if the transition matrix is sparse `predict()` and `route()`
both calculate on sparse objects.  This should speed those two functions up significantly.

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


# BirdFlowR 0.0.0.9028  2023-03-08

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
  - lookup_transitions(start, end, bf, direction) -> lookup_transition(x, start, end, direction) 
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

* Added "@keyword internal" to documentation for all non-exported functions.  This removes the documentation from the package manual and index, but it's still accessible with ?function_name. 


# BirdFlowR 0.0.0.9016  2023-02-27

* Added package down. Starting to use [semantic versioning](https://semver.org/).

# BirdflowR 0.0.0.9017  2023-02-27

* Fix bug introduced by ebirdst 2.2021.0 (switch from raster to terra)
[#17](https://github.com/birdflow-science/BirdFlowR/issues/17).

# BirdFlowR 0.0.0.9003  2023-02-27

* Updated installation instructions.  Closing [#11](https://github.com/birdflow-science/BirdFlowR/issues/11).

* Added docker file. [Usage instructions.](https://github.com/birdflow-science/BirdFlowR/pull/15#issuecomment-1445152787) 

# BirdFlowR 0.0.0.9002  2023-02-23

* Added "biocViews:" before "Imports:" in DESCRIPTION so that devtools can find
and install rhdf5 (from Bioconductor) while installing the packages BirdFlowR 
needs. [#13](https://github.com/birdflow-science/BirdFlowR/issues/13) 

* Vignette now attempts to load rnaturalearthdata with utils::install.packages() 
instead of devtools::install_cran() in attempt to fix [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 

* Updated get_naturalearth() so that it handles some cases in which it previously
failed.  In particular extents that span more than 180 deg of longitude, and 
extents that cross the 180 deg. meridian that defines the edge of the WGS84 
projection both now work. The function still doesn't handle polar projections or
global extents in most projections. Fixes [#14](https://github.com/birdflow-science/BirdFlowR/issues/14) 

# BirdFlowR 0.0.0.9001   2023-02-21
* Switched order of package installation in readme and added installation of
rnaturalearthdata to vignette [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 

* Added a `NEWS.md` file to track changes to the package.
