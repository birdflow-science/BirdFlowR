# nolint start: line_length_linter.
#' Prepare eBird Status and Trends data for BirdFlow model fitting
#'
#' Write a template BirdFlow object to an hdf5 file based on distribution data
#' downloaded with \pkg{ebirdst}. The object is complete except for marginals
#' and transitions.  Use `...` to truncate the model to part of the year.
#'
#' @section {Maximum number of parameters}:
#'
#' The maximum number of parameters that can be fit is machine dependent.
#' 2023-02-10 we tested under different resolutions with "amewoo" and
#' identified bounds on the maximum.
#'
#'| Machine | GPU Ram (GB) | Lower Bound (worked) | Upper Bound (failed)| Parameters / GB  |
#'| ------- | ------------ | -------------------- | ------------------- | ------------|
#'|titanx GPU | 12GB  | 306804561            | 334693725           | 25567047    |
#'| m40 GPU  | 24GB   | 557395226            | 610352178           | 23224801    |
#'
#' The number of parameters is the number of unmasked cells for the first
#' timestep + the total number of cells in the marginals which is calculated
#' from the dynamic mask.
#'
#' If `gpu_ram` is used (and not `res` or `max_parameters` ) than
#' `max_parameters` is set to `23,224,801 * gpu_ram` (lower of two values in
#' table above).
#'
#' The heuristic to determine resolution given a maximum number of parameters
#' must estimate the number of cells covered by the data
#' at a different resolution, a noisy process, so it iteratively tries to find
#'  the smallest resolution that doesn't exceed `max_params` and then rounds to
#'  a slightly larger resolution (fewer parameters).
#'
#' @param species A species in any format accepted by [ebirdst::get_species()]
#' @param out_dir Output directory, files will be written here. Required unless
#'   `hdf5` is FALSE.  File names created here will incorporate
#'   the species code, resolution, and eBird version year.
#' @param res The target resolution of the BirdFlow model in kilometers. If
#'   `res` is NULL (default) then a resolution that results in less than `max_params`
#'   parameters will be used, while also minimizing the resolution and limiting
#'   the number of significant digits.
#' @param hdf5 If TRUE (default) an hdf5 file will be exported.
#' @param overwrite If TRUE (default) any pre-existing output files will be
#'   overwritten. If FALSE pre-existing files will result in an error.
#' @param crs Coordinate reference system (CRS) to use.  Defaults to the custom
#'   projection eBird has assigned to this species - see
#'   [ebirdst::load_fac_map_parameters()]). It will be interpreted by
#'   [terra::crs()] to generate a well known text representation of the CRS.
#' @param clip A polygon or the path to a file containing a polygon. It must
#'   have a CRS and should either be a [SpatVector()][terra::SpatVector] object
#'   or produce one when called with [vect(clip)][terra::vect()]
#' @param max_params The maximum number of fitted parameters that the BirdFlow
#'   model should contain. Ignored if `res` is not NULL.  Otherwise a resolution
#'   will be chosen that yields this many fitted parameters. See `gpu_ram` for
#'   the default way of setting `max_params` and `res`. Note: the reduction in
#'   parameters resulting from truncation (see `...`) is not factored into the
#'   calculation.
#' @param gpu_ram Gigabytes of ram on GPU machine that will fit the models.
#'   If `res` is NULL and `max_params` is NULL this is used to estimate
#'   `max_params`which is, in turn, used to determine the resolution. Ignored
#'   if either` res` or `max_params` is set.
#' @param skip_quality_checks If `TRUE` than preprocess the species even if
#'   not all of four ranges are modeled (under ebirdst 2021 version year) or for
#'   2022 and subsequent data versions if
#'   not all `<season>_quality` is higher than `min_season_quality` in
#'   [ebirdst_runs][ebirdst::ebirdst_runs]).
#' @param min_season_quality The minimum acceptable season quality when
#'   preprocessing eBird 2022 and subsequent versions. Used to check model
#'   quality using based on the four `<season>_model_quality` columns in
#'   [ebirdst_runs][ebirdst::ebirdst_runs] ignored with 2021 \pkg{ebirdst}
#'   version year.
#' @inheritDotParams lookup_timestep_sequence -x
#'
#' @return Returns a BirdFlow model object that lacks marginals, but is
#'   otherwise complete.  It is suitable for fitting with
#'   [BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  bf <- preprocess_species("amewoo", hdf5 = FALSE )
#'  plot_distr(get_distr(bf, c(1, 26)), bf = bf)
#'
#' # Create clip polygon as an sf object
#' # Use the extent rectangle but with western edge moved in
#' # The clip can be anything that terra::vect will process into a polygon
#' e <- ext(bf)
#' e[1] <- -1500000
#' coords <- matrix(c(e[1], e[3],
#'                    e[1], e[4],
#'                    e[2], e[4],
#'                    e[2], e[3],
#'                    e[1], e[3]), ncol = 2, byrow = TRUE)
#' sfc <- sf::st_sfc(sf::st_polygon(list(coords)), crs = crs(bf))
#' clip <- sf::st_sf(data.frame(id = 1, geom = sfc))
#'
#' bfc <- preprocess_species("amewoo", hdf5 = FALSE, clip = clip ) # with clip
#'
#'  plot_distr(get_distr(bfc, 1), bfc)
#'
#'
#' }
# nolint end
preprocess_species <- function(species = NULL,
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
                               ...

) {
  # ebirdst is listed under suggests so may not be installed.
  # I anticipate a group of users who work with fit models provided by the
  # BirdFlow team but don't fit their own models, and my initial installation
  # of ebirdst was tricky so I didn't want all users to need to import it.
  a <- requireNamespace("ebirdst",
                        versionCheck = list(op = ">=", version = "2.2021.0"))
  if (!a) {
    stop("Install ebirdst >= 2.2021.0 to use preprocess_species()")
  }

  # Define local variables
  st_year <- ebirdst::ebirdst_version()$version_year
  any_output <- hdf5
  max_param_per_gb <- birdflow_options("max_param_per_gpu_gb")
  project_method <- "bilinear"


  # Validate inputs
  if (is.null(species))
    stop("species cannot be NULL")

  if (length(species) != 1)
    stop("Can only preprocess one species at a time")

  if (is.na(species))
    stop("species cannot be NA")

  stopifnot(is.logical(hdf5),
            length(hdf5) == 1)

  # Handle "example_data"  and "yesap-example" as species
  # BirdFlowR excepts either as the example species input but changes
  # the download name to whichever is appropriate for the installed
  # ebirdst (it changed with 2022 data release)
  # download_species is used when downloading, loading raster, and writing files
  # but "yebsap" is used to look up species information
  if (species %in% c("example_data", "yebsap-example")) {
    bf_msg(
      "The example dataset does not represent a complete species range\n",
      "so should only used for demonstrating package functions.\n")
    download_species <-  ebirdst_example_species() # example for current ver.
    species <- "yebsap"  # used to look up metadata
    if (!is.null(res) && res < 27)
      stop("res must be at least 27 when working with the low resolution ",
           "example_data")
  } else {
    a <- ebirdst::get_species(species)
    if (is.na(a))
      stop('"', species, '" is not an eBird S&T species')
    species <- download_species <- a
  }

  if (!is.null(res)) {
    stopifnot(length(res) == 1)
    if (res < 3)
      stop("Resolution cannot be less than 3 km")
  }

  # Create empty BirdFlow object
  # This is a nested list of all the components but most are NA.
  export <- new_BirdFlow()
  export$trans <- NULL
  export$marginals <- NULL


  # Check for output directories.
  if (any_output) {
    if (is.null(out_dir))
      stop("Need an output directory. Please set out_dir.")
    out_dir <- gsub("/$|\\\\$", "", out_dir) # drop trailing slash
    if (!dir.exists(out_dir))
      stop("Output directory ", out_dir, " does not exist.")
  }

  #----------------------------------------------------------------------------#
  # format species metadata                                                 ####
  #----------------------------------------------------------------------------#
  er <- ebirdst::ebirdst_runs
  names(er)[names(er) == "is_resident"] <- "resident" # restore 2021 name
  spmd <- as.list(er[er$species_code == species, , drop = FALSE])

  bf_msg("Species resolved to: '", species, "' (", spmd$common_name, ")\n")

  # Reformat dates as strings
  date_to_char <- function(x) {
    if (inherits(x, "Date"))
      x <- as.character(x)
    return(x)
  }
  spmd <- lapply(spmd, date_to_char)

  # Check that ebirdst species data supports BirdFlow modeling
  if (spmd$resident)
    stop(spmd$common_name, " (", spmd$species_code, ") is a resident ",
         "(non-migratory) species and is therefore a poor candidate for ",
         "BirdFlow modeling.")

  # Restore formatting to ebirdst columns
  # in ebirdst 2.2021.1 all columns are stored as characters.
  # it was fixed in 2.2021.3 so this is now extra security against
  # future format changes
  logical_variables <- intersect(c("resident",
                                   "breeding_range_modeled",
                                   "nonbreeding_range_modeled",
                                   "postbreeding_migration_range_modeled",
                                   "prebreeding_migration_range_modeled"),
                                 names(spmd))

  numeric_variables <- intersect(c("breeding_quality",
                                   "nonbreeding_quality",
                                   "postbreeding_migration_quality",
                                   "prebreeding_migration_quality"),
                                 names(spmd))

  spmd[logical_variables] <- as.logical(spmd[logical_variables])
  spmd[numeric_variables] <- as.numeric(spmd[numeric_variables])


  # Check eBird data quality

  # used for check prior to ebirds 3.2022.0 and to drop these columns (any v.)
  model_coverage_variables <- c("breeding_range_modeled",
                                "nonbreeding_range_modeled",
                                "postbreeding_migration_range_modeled",
                                "prebreeding_migration_range_modeled")

  if (ebirdst_pkg_ver() < "3.2022.0") {
    if (!skip_quality_checks && ! all(unlist(spmd[model_coverage_variables])))
      stop("eBird status and trends models do not cover the full range for ",
           spmd$common_name, " (", spmd$species_code, ")")
  } else {
    # ebirdst >= 3.2022.0
    model_quality_variables <- c("breeding_quality",
                                 "nonbreeding_quality",
                                 "postbreeding_migration_quality",
                                 "prebreeding_migration_quality")
    if (!skip_quality_checks &&
        any(unlist(spmd[model_quality_variables]) < min_season_quality))
      stop("eBird status and trends model quality is less than ",
           min_season_quality,
           " in one or more seasons for ",
           spmd$common_name, " (", spmd$species_code, ")", sep = "")
  }

  # Drop the variables that aren't relevant to BirdFlow
  # * The model_coverage_variables were dropped from ebirdst with v 3.2022
  #   and have always been dropped from BirdFlow objects
  # * Resident information dropped b/c we only fit BirdFlow models to migrants
  spmd <- spmd[!names(spmd) %in% model_coverage_variables]
  spmd$resident <- NULL
  spmd$resident_quality <- NULL
  spmd$resident_end <- NULL
  spmd$resident_start  <- NULL

  # As of ebirdst 3.2022 we also need to drop trends variables
  spmd[grep("trends", names(spmd))] <- NULL
  spmd$rsquared <- NULL # trends fit quality
  spmd$beta0 <- NULL # trends model intercept

  # check contents against new_BirdFlow() for consistency
  stopifnot(all(names(spmd) == names(export$species)))

  # Save species metadata for export
  export$species <- spmd

  # Add ebirdst versions to  metadata
  v <- ebirdst::ebirdst_version()
  export$metadata$ebird_version_year <- v$version_year
  export$metadata$ebird_release_year <- v$release_year
  export$metadata$ebirdst_version <- as.character(ebirdst_pkg_ver())
  export$metadata$ebird_access_end_date <- as.character(v$access_end_date)
  export$metadata$birdflow_preprocess_date <- as.character(Sys.Date())
  export$metadata$birdflowr_preprocess_version <-
    as.character(utils::packageVersion("BirdFlowR"))

  #----------------------------------------------------------------------------#
  # Download abundance data                                                 ####
  # Download - requires setting code with set_ebirdst_access_key()
  # Saves to disk. Path for windows 10 was:
  #  ~\AppData\Roaming\R\data\R\ebirdst\2021\[species]
  #----------------------------------------------------------------------------#

  # Define patterns for selecting specific files to download for each resolution
  download_patterns <-
    as.list(paste0("_abundance_((lower)|(median)|(upper))_",
                   res_label(c("lr", "mr", "hr"))))
  names(download_patterns) <- c("lr", "mr", "hr")

  # Initially download just the Low resolution
  bf_suppress_msg({ # Conditionally suppress messages
    if (ebirdst_pkg_ver() < "3.2022.0") {
      sp_path <- ebirdst::ebirdst_download(
        download_species,
        pattern = download_patterns$lr)
    } else {
      sp_path <- ebirdst::ebirdst_download_status(
        download_species,
        pattern = download_patterns$lr)
    }
  })  # End conditional message suppression

  # Load map parameters and set crs
  if (ebirdst_pkg_ver() < "3.2022.0") {
    mp <- ebirdst::load_fac_map_parameters(path = sp_path)
  } else {
    mp <- ebirdst::load_fac_map_parameters(species = download_species)
  }

  if (is.null(crs)) {
    crs <- terra::crs(mp$custom_projection)
  } else {
    crs <- terra::crs(crs)
  }

  # Format and reproject clip
  if (!is.null(clip)) {
    if (!inherits(clip, "SpatVector")) {
      clip <- terra::vect(clip)
    }
    clip <- terra::project(clip, crs)
  }

  #----------------------------------------------------------------------------#
  # Determine model resolution                                              ####
  #----------------------------------------------------------------------------#
  res <- determine_resolution(sp_path = sp_path,
                              res = res,
                              max_params = max_params,
                              gpu_ram = gpu_ram,
                              clip = clip,
                              crs = crs,
                              download_species = download_species,
                              project_method = project_method)

  #----------------------------------------------------------------------------#
  # Set output paths  (depends on resolution)                               ####
  #----------------------------------------------------------------------------#
  if (any_output) {
    out_base <-
      file.path(out_dir, paste0(download_species, "_", st_year, "_", res, "km"))
    if (!is.null(clip))
      out_base <- paste0(out_base, "_clip")

    paths <- list()
    if (hdf5) {
      paths <- c(paths, list(hdf5 = paste0(out_base, ".hdf5")))
    }

    exist <- file.exists(unlist(paths))
    if (!overwrite && any(exist)) {
      stop("Prexisiting output files: ",
           paste(unlist(paths[exist]), collapse = ", "),
           " Set overwrite = TRUE to overwrite.")
    }
    rm(exist, out_base)
  }

  #----------------------------------------------------------------------------#
  #  Make distributions, mask, and confidence intervals
  #----------------------------------------------------------------------------

  # Transform and change resolution
  a <- process_rasters(res = res,
                       crs = crs,
                       download_species = download_species,
                       sp_path = sp_path,
                       clip = clip,
                       project_method = project_method,
                       download_patterns = download_patterns)
  mask <- a$mask

  # Save distribution and confidence intervals to export object
  export$distr <- a$distr
  export$uci <- a$uci
  export$lci <- a$lci

  #----------------------------------------------------------------------------#
  #  Define geom                                                            ####
  #----------------------------------------------------------------------------#

  # Create geometry object describing this BirdFlow model
  geom <- list(nrow = nrow(mask),
               ncol = ncol(mask),
               res = res(mask),
               ext = as.vector(ext(mask)),
               crs = crs(mask),
               mask = NA)
  m <- terra::values(mask)
  m <- matrix(as.logical(m), nrow = nrow(mask), ncol = ncol(mask), byrow = TRUE)
  geom$mask <- m
  export$geom <- geom

  # Update metadata
  export$metadata$has_distr <- TRUE
  export$metadata$n_active <- sum(m)
  export$metadata$n_transitions <-  ncol(export$distr)
  export$metadata$timestep_padding <- nchar(n_transitions(export))

  #----------------------------------------------------------------------------#
  #  Define dates                                                           ####
  #----------------------------------------------------------------------------#

  # Save to export object
  export$dates <- make_dates()
  export$metadata$n_timesteps <- length(unique(export$dates$date))

  #----------------------------------------------------------------------------#
  #  Add dynamic_mask and distances                                         ####
  #----------------------------------------------------------------------------#
  export$distances <- great_circle_distances(export) |>
    shorten_distance_matrix()
  export$geom$dynamic_mask <- export$distr > 0

  #----------------------------------------------------------------------------#
  #  Print details                                                          ####
  #----------------------------------------------------------------------------#
  n_params <- n_parameters(export)
  message <- paste0("Model has:\n\t", sum(m), " active cells,\n\t",
                    n_transitions(export), " transitions,",
                    " and\n\t", format(n_params, big.mark = ","), " parameters")
  if (!is.null(max_params)) {
    pct_max_params <- n_params / max_params * 100
    message <- paste0(message, ", ", round(pct_max_params, 1),
                      "% of maximum parameters", sep = "")
  }
  message <- paste0(message, "\n",
                    round(n_params / max_param_per_gb, 1),
                    "gb of GPU ram required to fit model.\n")
  bf_msg(message)

  #----------------------------------------------------------------------------#
  # Truncate
  #----------------------------------------------------------------------------#
  truncated <- !all(seq_len(n_timesteps(export)) %in%
                      lookup_timestep_sequence(x = export, ...))

  # Create standard timestep based column names in distributions and CIs
  colnames(export$uci) <- colnames(export$lci) <-
    colnames(export$distr) <- colnames(export$geom$dynamic_mask) <-
    paste0("t", seq_len(ncol(export$distr)))

  if (truncated) {
    truncated <- TRUE
    export <- truncate_birdflow(export, ...)
    bf_msg("After truncation model has:",  n_parameters(export), "parameters\n")

  }

  #----------------------------------------------------------------------------#
  #  Validate                                                               ####
  #----------------------------------------------------------------------------#
  validate_BirdFlow(export, allow_incomplete = TRUE)

  #----------------------------------------------------------------------------#
  # Make cyclical
  #----------------------------------------------------------------------------#
  # Duplicate first distribution, first dynamic mask, and corresponding dates
  # so full cycle is fit
  # Skip if truncated because truncated models cannot be cyclical.
  if (!truncated) {

    # Dates
    dates <- export$dates

    # Extract first date and reinsert as timestep 53
    first <- dates[1, , drop = FALSE]
    timestep_col <- ifelse("timestep" %in% names(dates), "timestep", "interval")
    first[[timestep_col]] <- dates[nrow(dates), timestep_col] + 1  # ts53, wk 1
    dates <- rbind(dates, first)
    export$dates <- dates
    export$metadata$n_timesteps <- length(unique(dates$date))

    # distr append first column onto end so we have full cycle of transitions
    distr <- export$distr
    uci <- export$uci
    lci <- export$lci
    dynamic_mask <- export$geom$dynamic_mask

    distr <- cbind(distr, distr[, 1, drop = FALSE])
    uci <- cbind(uci, uci[, 1, drop = FALSE])
    lci <- cbind(lci, lci[, 1, drop = FALSE])
    dynamic_mask <- cbind(dynamic_mask, dynamic_mask[, 1, drop = FALSE])
    colnames(uci) <- colnames(lci) <-
      colnames(distr) <- colnames(dynamic_mask) <-
      paste0("t", seq_len(ncol(distr)))

    export$distr <- distr
    export$uci <- uci
    export$lci <- lci
    export$geom$dynamic_mask <- dynamic_mask

  }

  #----------------------------------------------------------------------------#
  #   # Write HDF5                                                          ####
  #----------------------------------------------------------------------------#
  if (hdf5) {
    export_birdflow(bf = export, file = paths$hdf5, overwrite = overwrite)
  }

  # invisibly return exported BirdFlow model
  invisible(export)

}
