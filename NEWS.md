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
