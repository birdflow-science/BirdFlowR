# Package index

## Core functions

The primary entry points for loading a fitted model, projecting
distributions, and simulating routes.

- [`load_collection_index()`](https://birdflow-science.github.io/BirdFlowR/reference/load_collection_index.md)
  : Load the index to a remote model collection
- [`load_model()`](https://birdflow-science.github.io/BirdFlowR/reference/load_model.md)
  : Load BirdFlow models from a collection
- [`predict(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/predict.BirdFlow.md)
  : Predict bird distributions
- [`predict_between()`](https://birdflow-science.github.io/BirdFlowR/reference/predict_between.md)
  : Predict bird distributions conditioned on known locations
- [`route()`](https://birdflow-science.github.io/BirdFlowR/reference/route.md)
  : Generate BirdFlow routes
- [`route_between()`](https://birdflow-science.github.io/BirdFlowR/reference/route_between.md)
  : Generate BirdFlow routes between known locations
- [`calc_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_bmtr.md)
  : Estimate BirdFlow Migration Traffic Rate (BMTR)
- [`rasterize_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize_bmtr.md)
  : Rasterize BMTR Data
- [`calc_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_movement_vectors.md)
  : Calculate the average movement for cells in a BirdFlow model

## Working with distributions

Retrieve, create, sample, and convert the species distributions that a
BirdFlow model represents.

- [`get_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/get_distr.md)
  : Extract distributions from BirdFlow models
- [`as_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/as_distr.md)
  : Convert to a BirdFlow distribution
- [`sample_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/sample_distr.md)
  : Sample locations from a distribution
- [`rasterize_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md)
  [`rast(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/rasterize.md)
  : Convert a BirdFlow distribution into a raster
- [`flatten_raster()`](https://birdflow-science.github.io/BirdFlowR/reference/flatten_raster.md)
  : Convert a raster bird distribution into its flattened, vector
  equivalent

## Retrieving information

Accessors for a model’s dates, geometry, and metadata.

### Temporal

- [`get_dates()`](https://birdflow-science.github.io/BirdFlowR/reference/get_dates.md)
  : Retrieve dates component of a BirdFlow model
- [`lookup_date()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_date.md)
  : Retrieve date associated with timesteps, transitions, or marginals
- [`lookup_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep.md)
  : Lookup timestep
- [`lookup_timestep_sequence()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_timestep_sequence.md)
  : Lookup a sequence of timesteps
- [`lookup_season_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/lookup_season_timesteps.md)
  : Lookup breeding, non-breeding, or migration season timesteps
- [`nrow(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ncol(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`dim(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`crs(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ext(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`res(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xres(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`yres(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xmin(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ymin(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xmax(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ymax(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`is_cyclical()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_parameters()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  : Dimensions of a BirdFlow object

### Spatial

- [`get_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_mask.md)
  : Extract mask from BirdFlow model
- [`get_dynamic_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/get_dynamic_mask.md)
  : Get the dynamic mask from a BirdFlow object
- [`get_clip()`](https://birdflow-science.github.io/BirdFlowR/reference/get_clip.md)
  : Retrieve the clip polygon used during preprocessing
- [`is_clipped()`](https://birdflow-science.github.io/BirdFlowR/reference/is_clipped.md)
  : Test whether a BirdFlow model was clipped during preprocessing
- [`is_location_valid()`](https://birdflow-science.github.io/BirdFlowR/reference/is_location_valid.md)
  [`is_distr_valid()`](https://birdflow-science.github.io/BirdFlowR/reference/is_location_valid.md)
  : Are locations and distributions covered by the model for the given
  time
- [`get_naturalearth()`](https://birdflow-science.github.io/BirdFlowR/reference/get_naturalearth.md)
  [`get_states()`](https://birdflow-science.github.io/BirdFlowR/reference/get_naturalearth.md)
  [`get_coastline()`](https://birdflow-science.github.io/BirdFlowR/reference/get_naturalearth.md)
  [`get_countries()`](https://birdflow-science.github.io/BirdFlowR/reference/get_naturalearth.md)
  : Retrieve, crop, and transform Natural Earth data
- [`compareGeom(`*`<BirdFlow>`*`,`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/compareGeom-BirdFlow.md)
  [`compareGeom(`*`<SpatRaster>`*`,`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/compareGeom-BirdFlow.md)
  [`compareGeom(`*`<BirdFlow>`*`,`*`<SpatRaster>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/compareGeom-BirdFlow.md)
  : BirdFlow compareGeom methods
- [`st_bbox(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/st_bbox.BirdFlow.md)
  : Get the bounding box of a BirdFlow model
- [`st_crs(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/st_crs.BirdFlow.md)
  : Get the coordinate reference system of a BirdFlow model
- [`nrow(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ncol(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`dim(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`crs(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ext(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`res(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xres(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`yres(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xmin(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ymin(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xmax(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ymax(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`is_cyclical()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_parameters()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  : Dimensions of a BirdFlow object

### Metadata

- [`get_metadata()`](https://birdflow-science.github.io/BirdFlowR/reference/get_metadata.md)
  : Retrieve BirdFlow metadata
- [`species_info()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
  [`species()`](https://birdflow-science.github.io/BirdFlowR/reference/species_info.md)
  : Retrieve species information from a BirdFlow model
- [`has_marginals()`](https://birdflow-science.github.io/BirdFlowR/reference/has.md)
  [`has_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/has.md)
  [`has_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/has.md)
  [`has_dynamic_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/has.md)
  : Does a BirdFlow object have certain components
- [`nrow(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ncol(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`dim(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_timesteps()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_active()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`crs(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ext(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`res(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xres(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`yres(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xmin(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ymin(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`xmax(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`ymax(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`is_cyclical()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  [`n_parameters()`](https://birdflow-science.github.io/BirdFlowR/reference/dimensions.md)
  : Dimensions of a BirdFlow object
- [`print(`*`<BirdFlow>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/print.BirdFlow.md)
  : Print BirdFlow models

### Other

- [`get_marginal()`](https://birdflow-science.github.io/BirdFlowR/reference/get_marginal.md)
  : Return a marginal matrix from a BirdFlowR model
- [`get_transition()`](https://birdflow-science.github.io/BirdFlowR/reference/get_transition.md)
  : Return a transition matrix from a BirdFlowR model
- [`get_loss()`](https://birdflow-science.github.io/BirdFlowR/reference/get_loss.md)
  : Get loss values for each step in the model fitting process
- [`get_ebird_coverage()`](https://birdflow-science.github.io/BirdFlowR/reference/get_ebird_coverage.md)
  : Get eBird model coverage from a BirdFlow model

## Routes and interval data

Constructors, I/O, and validators for tracked and synthetic route data
(`Routes` / `BirdFlowRoutes` / `BirdFlowIntervals`).

- [`as_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_routes.md)
  :

  Convert a data frame into a `Routes` object

- [`as_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_routes.md)
  : Convert Routes to BirdFlowRoutes

- [`as_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_birdflow_intervals.md)
  :

  Create `BirdFlowIntervals` objects

- [`snap_to_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/snap_to_birdflow.md)
  : Assign cells and timesteps to coordinates and dates

- [`` `[`( ``*`<Routes>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/subset_routes.md)
  [`` `[`( ``*`<BirdFlowRoutes>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/subset_routes.md)
  [`` `[`( ``*`<BirdFlowIntervals>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/subset_routes.md)
  :

  Subset `Routes`, `BirdFlowRoutes`, or `BirdFlowIntervals` objects by
  row

- [`sort_by_id_and_dates()`](https://birdflow-science.github.io/BirdFlowR/reference/sort_by_id_and_dates.md)
  : Sort Routes by ID and Date

- [`add_stay_id()`](https://birdflow-science.github.io/BirdFlowR/reference/add_stay_id.md)
  : Add Stay IDs

- [`add_stay_id_with_varied_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/add_stay_id_with_varied_intervals.md)
  : Add Stay IDs with Temporal Thresholds

- [`write_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/hdf5_serialization.md)
  [`read_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/hdf5_serialization.md)
  [`read_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/hdf5_serialization.md)
  [`write_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/hdf5_serialization.md)
  : Read and write routes and intervals

- [`print(`*`<Routes>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/print.Routes.md)
  : Print Routes and BirdFlowRoutes objects

- [`print(`*`<BirdFlowIntervals>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/print.BirdFlowIntervals.md)
  :

  Print a `BirdFlowIntervals` Object

- [`st_as_sf(`*`<BirdFlowRoutes>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/st_as_sf.BirdFlowRoutes.md)
  : Convert BirdFlowRoutes to an sf object

- [`validate_route_df()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_interval_df()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_birdflow_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_birdflow_intervals()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  [`validate_BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/object_validators.md)
  : Object Validators

## Evaluating models against tracking data

- [`distribution_performance()`](https://birdflow-science.github.io/BirdFlowR/reference/distribution_performance.md)
  : Evaluate a BirdFlow models ability to recreate training
  distributions

- [`calc_interval_metrics()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_interval_metrics.md)
  : Calculate interval metrics

- [`great_circle_distances()`](https://birdflow-science.github.io/BirdFlowR/reference/great_circle_distances.md)
  : Calculate great circle distances among BirdFlow object cells

- [`visualize_distance_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/visualize_distance_weights.md)
  :

  Visualize the spread kernel used by
  [`calc_dist_weights()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_dist_weights.md)

## Coordinates and indice conversions

Convert among the location index, row/column, model (x, y), and (lat,
lon) representations of a cell.

- [`x_to_col()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`y_to_row()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`row_to_y()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`col_to_x()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`i_to_rc()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`i_to_row()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`i_to_col()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`i_to_x()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`i_to_y()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`i_to_xy()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`rc_to_i()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`xy_to_i()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`latlon_to_xy()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  [`xy_to_latlon()`](https://birdflow-science.github.io/BirdFlowR/reference/index_conversions.md)
  : Functions to convert among spatial indices
- [`dmi_to_i()`](https://birdflow-science.github.io/BirdFlowR/reference/dynamic_mask_index_conversions.md)
  [`i_to_dmi()`](https://birdflow-science.github.io/BirdFlowR/reference/dynamic_mask_index_conversions.md)
  : Convert between dynamic mask index and static location index

## Visualization

Static plots and animations of distributions, bmtr, movement vectors,
routes, and model loss.

- [`plot_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_distr.md)
  : Plot distributions
- [`animate_distr()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_distr.md)
  : Animate distributions
- [`plot_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_bmtr.md)
  : Plot BirdFlow Migration Traffic Rate (BMTR)
- [`animate_bmtr()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_bmtr.md)
  : Animate BirdFlow Migration Traffic Rate (BMTR)
- [`plot_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_movement_vectors.md)
  : Plot bird movement vectors
- [`animate_movement_vectors()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_movement_vectors.md)
  : Animate movement vectors
- [`plot_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md)
  [`plot(`*`<BirdFlowRoutes>`*`)`](https://birdflow-science.github.io/BirdFlowR/reference/plot_routes.md)
  : Plot Routes
- [`animate_routes()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_routes.md)
  : Animate routes
- [`plot_loss()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_loss.md)
  : Plot changes in component and total loss during model fitting

## Utilities

Building, importing/exporting, and manipulating BirdFlow model objects.

- [`preprocess_species()`](https://birdflow-science.github.io/BirdFlowR/reference/preprocess_species.md)
  : Prepare eBird Status and Trends data for BirdFlow model fitting
- [`export_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md)
  [`import_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/export_import_birdflow.md)
  : Export and import BirdFlow models in HDF5 format
- [`export_rasters()`](https://birdflow-science.github.io/BirdFlowR/reference/export_rasters.md)
  : export distributions and masks from a BirdFlow object to raster
  files.
- [`make_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/make_mask.md)
  : Internal function to determine the extent of data in eBird rasters.
- [`add_dynamic_mask()`](https://birdflow-science.github.io/BirdFlowR/reference/add_dynamic_mask.md)
  : Convert a BirdFlow object without a dynamic mask into one with a
  dynamic mask
- [`birdflow_options()`](https://birdflow-science.github.io/BirdFlowR/reference/birdflow_options.md)
  : Set and retrieve BirdFlowR options
- [`sparsify()`](https://birdflow-science.github.io/BirdFlowR/reference/sparsify.md)
  : sparsify BirdFlow models
- [`build_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/build_transitions.md)
  [`drop_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/build_transitions.md)
  : Add or drop transition matrices
- [`combine_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/combine_transitions.md)
  : Function to combine a sequence of transition matrices into one
- [`as_transitions()`](https://birdflow-science.github.io/BirdFlowR/reference/as_transitions.md)
  : Convert a sequence of timesteps into a sequence of transition names
- [`shrink_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/shrink_birdflow.md)
  : Shrink BirdFlow extent
- [`truncate_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/truncate_birdflow.md)
  : Truncate the timespan of a BirdFlow model
- [`extend_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/extend_birdflow.md)
  : Extend BirdFlow extent
- [`validate_BirdFlow()`](https://birdflow-science.github.io/BirdFlowR/reference/validate_BirdFlow.md)
  : Function to validate a BirdFlow object
- [`pad_timestep()`](https://birdflow-science.github.io/BirdFlowR/reference/pad_timestep.md)
  : pad timesteps from BirdFlow models
- [`upgrade_birdflow()`](https://birdflow-science.github.io/BirdFlowR/reference/upgrade_birdflow.md)
  : Upgrade an older BirdFlow object to the current in-memory format

## Deprecated

- [`route_migration()`](https://birdflow-science.github.io/BirdFlowR/reference/route_migration.md)
  : Deprecated function to generate migration routes from a BirdFlow
  model

- [`evaluate_performance()`](https://birdflow-science.github.io/BirdFlowR/reference/evaluate_performance.md)
  : Evaluate BirdFlow model performance

- [`calc_flux()`](https://birdflow-science.github.io/BirdFlowR/reference/calc_flux.md)
  : Calculate Bird Flow Migration Traffic Rate

- [`plot_flux()`](https://birdflow-science.github.io/BirdFlowR/reference/plot_flux.md)
  : Plot Bird Flow Migration Traffic Rate (BMTR)

- [`animate_flux()`](https://birdflow-science.github.io/BirdFlowR/reference/animate_flux.md)
  : Animate Bird Flow Migration Traffic Rate (BMTR)

- [`interval_log_likelihood()`](https://birdflow-science.github.io/BirdFlowR/reference/interval_log_likelihood.md)
  : Calculate log likelihoods of observed bird movements

- [`Routes()`](https://birdflow-science.github.io/BirdFlowR/reference/Routes.md)
  :

  Deprecated function to convert a data frame into a `Routes` object

- [`BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/BirdFlowIntervals.md)
  : Deprecated function to make a BirdFlowIntervals object from a data
  frame

- [`as_BirdFlowRoutes()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowRoutes.md)
  : Deprecated function to convert Routes to BirdFlowRoutes

- [`as_BirdFlowIntervals()`](https://birdflow-science.github.io/BirdFlowR/reference/as_BirdFlowIntervals.md)
  : Deprecated function to convert BirdFlowRoutes to BirdFlowIntervals
