#' prepare eBird Status and Trends data for BirdFlow model fitting
#'
#' Write a template BirdFlow object to an hdf5 file based on distribution data
#' downloaded with \pkg{ebirdst}. The object is complete except for marginals
#' and transitions.
#'
#' @section {Maximum number of parameters}:
#'
#' The maximum number of parameters that can be fit is machine dependent.
#' 2023-02-10 we tested under different resolutions with "amewoo" and
#' identified bounds on the maximum.
#'
#'| Machine | GPU Ram (GB) | Lower Bound (worked) | Upper Bound (failed)| Params / GB  |
#'| ------- | ------------ | -------------------- | ------------------- | ------------|
#'|titanx gpu | 12GB  | 306804561            | 334693725           | 25567047    |
#'| m40 gpu  | 24GB   | 557395226            | 610352178           | 23224801    |
#'
#' The number of parameters is the number of unmasked cells for the first
#' timestep + the total number of cells in the marginals which is calculated
#' from the dynamic mask.
#'
#' If `gpu_ram` is used (and not `res` or `max_parameters` ) than  `max_parameters` is
#' set to `23,224,801 * gpu_ram` (lower of two values in table above).
#'
#' The heuristic to determine resolution given a maximum number of parameters
#' must estimate the number of cells covered by the data
#' at a different resolution, a noisy process, so it iteratively tries to find
#'  the smallest resolution that doesn't exceed `max_params` and then rounds to
#'  a slightly larger resolution (fewer parameters).
#'
#' @param species a species in any format accepted by [ebirdst::get_species()]
#' @param out_dir output directory, files will be written here. Required unless
#'   both `tiff` and `hdf5` are TRUE.  File names created here will incorporate
#'   the species code, resolution, and eBird version year.
#' @param res the target resolution of the BirdFlow model in kilometers. If
#'   `res` is omitted than a resolution that results in less than`max_params`
#'   parameters will be used, while also minimizing the resolution and limiting
#'   the number of significant digits.
#' @param hdf5 if TRUE (default) an hdf5 file will be exported.
#' @param tiff set to TRUE to export geoTIFF files. They are not needed to fit the BirdFlow model.
#' @param overwrite if TRUE (default) any pre-existing output files will be
#'   overwritten. If FALSE pre-existing files will result in an error.
#' @param crs coordinate reference system (CRS) to use.  Defaults to the custom
#'   projection eBird has assigned to this species - see
#'   [ebirdst::load_fac_map_parameters()]). It will be interpreted by
#'   [terra::crs()] to generate a well known text representation of the CRS.
#' @param clip a polygon or the path to a file containing a polygon. It must
#'   have a CRS and should either be a [SpatVector()][terra::SpatVector] object
#'   or or produce one when called with [vect(clip)][terra::vect()]
#' @param max_params the maximum number of fitted parameters that the BirdFlow
#'   model should contain. Ignored if `res` is set.  Otherwise a resolution
#'   will be chosen that yields this many fitted parameters. See `gpu_ram` for
#'   the default way of setting `max_params` and `res`.
#' @param gpu_ram Gigabytes of ram on GPU machine that will fit the models.
#'   If `res` and `max_params` are both missing this is used to estimate
#'   `max_params`which is, in turn, used to determine the resolution. Ignored
#'   if either` res` or `max_params` is set.
#' @param skip_quality_checks If `TRUE` than preprocess the species even if
#'   not all of four ranges are modeled (based on
#'   [ebirdst_runs()][ebirdst::ebirdst_runs()]).
#' @param dummy_dynamic_mask set to TRUE to force all cells in the dynamic mask
#'   to TRUE. This creates a BirdFlow object with a dynamic mask component
#'   that is compatible with the current BirdFlowR package but that mimics the
#'   older  predynamic mask BirdFlow models.  If fit the resulting object will
#'   have transitions at every timestep among all active cells.  The only
#'   reason to set this to TRUE is for comparsion testing and quality control
#'   during the transition, and this parameter may eventually be dropped.
#'
#' @return returns a BirdFlow model object that lacks marginals, but is
#'   otherwise complete.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  bf <- preprocess_species("amewoo", tiff = FALSE, hdf5 = FALSE )
#'  plot(rasterize_distr(get_distr( bf, c(1, 26)), bf))
#'
#' # Create clip polgyon as an sf object
#' # Use the extent rectangle but with western edge moved in
#' # The clip can be anything that terra::vect will process into a polygon
#' e <- ext(bf)
#' e[1] <- -1500000
#' coords <- matrix(c(e[1], e[3],
#'                    e[1], e[4],
#'                    e[2], e[4],
#'                    e[2], e[3],
#'                    e[1], e[3]), ncol = 2, byrow = TRUE)
#' sfc <- st_sfc(st_polygon(list(coords)), crs = crs(bf))
#' clip <- st_sf(data.frame(id = 1, geom = sfc))
#'
#' bfc <- preprocess_species("amewoo", tiff = FALSE,
#'                          hdf5 = FALSE, clip = clip ) # clipped bird flow
#'
#'  plot(rasterize_distr(get_distr(bfc, 1), bfc))
#'
#'
#' }
preprocess_species <- function(species,
                               out_dir,
                               res,
                               hdf5 = TRUE,
                               tiff = FALSE,
                               overwrite = TRUE,
                               crs,
                               clip,
                               max_params,
                               gpu_ram = 12,
                               skip_quality_checks = FALSE,
                               dummy_dynamic_mask = FALSE

){

  # ebirdst is listed under suggests so may not be installed.
  # I anticipate a group of users who work with fit models provided by the
  # BirdFlow team but don't fit their own models, and my initial installation
  # of ebirdst was tricky so I didn't want all users to need to import it.
  a <- requireNamespace("ebirdst", versionCheck = list(op =">=", version = "2.2021.0"))
  if(!a){
    stop("Install ebirdst >= 2.2021.0 to use preprocess_species()")
  }

  # Validate inputs
  if(length(species) != 1)
    stop("Can only preprocess one species at a time")
  stopifnot( is.logical( tiff ),
             is.logical( hdf5 ),
             length( tiff ) == 1,
             length( hdf5 ) == 1 )

  # Handle "example_data" as a species
  # use "example_data" when downloading, loading raster, and writing files
  # but "yebsap" for looking up species information
  if(species == "example_data"){
    cat("The example datset does not represet a complete species range so\n",
        "should only used for demonstrating package functions.\n", sep ="")
    download_species <- "example_data"
    species <- "yebsap"
    if(!missing(res) && res < 27)
      stop("res must be at least 27 when working with the low resolution example_data")
  } else {
    species <- ebirdst::get_species(species)
    download_species <- species
  }

  if(!missing(res)){
    stopifnot(length(res) == 1)
    if (res < 3)
      stop("Resolution cannot be less than 3 km")
  }

  # Define local variables
  st_year <- ebirdst::ebirdst_version()$version_year
  verbose <- birdflow_options("verbose")
  any_output <- hdf5 || tiff
  max_param_per_gb <- birdflow_options("max_param_per_gpu_gb") # Relationship between parameters and GPU_ram


  # Create empty BirdFlow object
  # This is a nested list of all the components but most are NA.
  export <- new_BirdFlow()
  export$trans <- NULL
  export$marginals <- NULL


  # Check for output directories.
  if(any_output){
    if(missing(out_dir))
      stop("Need an output directory. Please set out_dir.")
    out_dir <- gsub("/$|\\\\$", "", out_dir) # drop trailing slash
    if(!dir.exists(out_dir))
      stop("Output directory ", out_dir, " does not exist.")
  }

  #----------------------------------------------------------------------------#
  # format species metadata                                                 ####
  #----------------------------------------------------------------------------#
  er <- ebirdst::ebirdst_runs
  spmd <- as.list(er[er$species_code == species, , drop = FALSE])


  if(verbose)
    cat("Species resolved to: '", species, "' (", spmd$common_name, ")\n",
        sep ="")

  # Reformat dates as strings
  date_to_char <- function(x){
    if(inherits(x, "Date"))
      x <- as.character(x)
    return(x)
  }
  spmd <- lapply(spmd, date_to_char)

  # Check that ebirdst species data supports BirdFlow modeling
  if(spmd$resident)
    stop(spmd$common_name, " (", spmd$species_code, ") is a resident ",
         "(non-migratory) species and is therefore a poor candidate for ",
         "BirdFlow modeling.")

  # Restore formatting to ebirdst columns
  # in ebirdst 2.2021.1 all columns are stored as characters.
  #  It's fixed by 2.2021.3 but CRAN is still on 2.2021.1
  logical_variables <- c("resident",
                         "breeding_range_modeled",
                         "nonbreeding_range_modeled",
                         "postbreeding_migration_range_modeled",
                         "prebreeding_migration_range_modeled")

  numeric_variables <- c("breeding_quality",
                         "nonbreeding_quality",
                         "postbreeding_migration_quality",
                         "prebreeding_migration_quality")

  spmd[logical_variables] <- as.logical(spmd[logical_variables] )
  spmd[numeric_variables] <- as.numeric(spmd[numeric_variables] )



  model_coverage_variables <- c("breeding_range_modeled",
                                "nonbreeding_range_modeled",
                                "postbreeding_migration_range_modeled",
                                "prebreeding_migration_range_modeled")

  if(!skip_quality_checks && ! all( unlist( spmd[model_coverage_variables]) ) )
    stop("eBird status and trends models do not cover the full range for ",
         spmd$common_name, " (", spmd$species_code, ")")

  # Drop the variables that aren't relevant to BirdFlow
  # The model_coverage_variables because they are all TRUE (verified above)
  # resident information because we only fit BirdFlow models to migrants
  spmd <- spmd[!names(spmd) %in% model_coverage_variables ]
  spmd$resident <- NULL
  spmd$resident_quality <- NULL
  spmd$resident_end <- NULL
  spmd$resident_start  <- NULL

  # check contents against new_BirdFlow() for consistency
  stopifnot(all(names(spmd) == names(export$species)))

  # Save species metadata for export
  export$species <- spmd

  # Add ebirdst versions to  metadata
  v <- ebirdst::ebirdst_version()
  export$metadata$ebird_version_year <- v$version_year
  export$metadata$ebird_release_year <- v$release_year
  export$metadata$ebird_access_end_date <- as.character(v$access_end_date)
  export$metadata$birdflow_preprocess_date <- as.character(Sys.Date())

  #----------------------------------------------------------------------------#
  # Download abundance data                                                 ####
  # Download - requires setting code with set_ebirdst_access_key()
  # Saves to disk. Path for windows 10 was:
  #  ~\AppData\Roaming\R\data\R\ebirdst\2021\[species]
  #----------------------------------------------------------------------------#

  # Define patterns for selecting specific files to download for each resolution
  download_patterns <-
    as.list(paste0("_abundance_((lower)|(median)|(upper))_", c("lr", "mr", "hr")))
  names(download_patterns) <- c("lr", "mr", "hr")

  # Initially download just the Low resolution
  sp_path <-  ebirdst::ebirdst_download(download_species,
                                        pattern = download_patterns$lr)

  # Load map parameters and set crs
  mp  <- ebirdst::load_fac_map_parameters(path = sp_path)
  if(missing(crs)){
    crs <- terra::crs(mp$custom_projection)
  } else {
    crs <- terra::crs(crs)
  }

  # Format and reproject clip
  if(!missing(clip)){
    if(!inherits(clip, "SpatVector")){
      clip <- terra::vect(clip)
    }
    clip <- terra::project(clip, crs)
  }


  #----------------------------------------------------------------------------#
  # Determine BirdFlow model resolution                                                    ####
  #   If the res argument isn't supplied the heuristic here attempts to set a
  #   resolution that will result in close to max_params (but stay under it)
  #   the total number of fitted parameters in the model. It rounds the
  #   resolution variably - rounding more for larger values.
  #   Note: it turned out to be really hard to anticipate how many cells would
  #     contain data after a resolution change.  The code estimates by
  #     calculating the area of the non-zero cells in the current resolution
  #     and then figures out the resolution where the number of cells required
  #     to cover that area matches our target number of parameters. However,
  #     it's a poor estimate because it ignores the
  #     fact that coarse cells along the edges overlap fine cells that contain
  #     a mix of no data and data.  The code here makes the estimate and then
  #     resamples to the estimate, evaluates the new number of cells (and
  #     thus parameters), and repeats until the estimate converges on a number
  #     of parameters between 90 and 100 % of the target number.
  #
  #   Feb 10 - added gpu_ram parameter that allows estimating max_params from
  #     the GB of ram on the machine used to fit the models. Internally, this
  #     is just a different way of setting the max_params before doing the work
  #     outlined above.
  #----------------------------------------------------------------------------#
  if(missing(res)){

    if(missing(max_params)){
      stopifnot( is.numeric(gpu_ram) | length(gpu_ram) == 1 | !is.na(gpu_ram) |  gpu_ram < 0 )
      max_params <- max_param_per_gb * gpu_ram
      if(verbose)
        cat("Setting max_params to ", max_params, " anticipating ", gpu_ram, " GB of GPU ram.\n" )
    }

    if(verbose)
      cat("Calculating resolution\n")
    # Load low res abundance data and calculate total areas birds occupy at any
    # time (active_sq_m)
    abunds <- ebirdst::load_raster("abundance",path = sp_path, resolution="lr")


    mask <- make_mask(x = abunds)


    if(!missing(clip)){
      clip2 <- terra::project(clip, terra::crs(mask))
      mask <- terra::mask(mask, clip2)
      mask[is.na(mask)] <- FALSE
      abunds <- terra::mask(abunds, clip2)

      if(verbose){
        # Calculate percent of density lost
        # will print after printing the resolved resolution
        sa <- sum(abunds)
        csa <- terra::mask(sa, clip2)
        tot_density <- sum(terra::values(sa), na.rm = TRUE)
        clipped_density <- sum(terra::values(csa), na.rm = TRUE)
        pct_lost <- round((tot_density - clipped_density)/tot_density * 100, 2)
        rm(sa, csa, tot_density, clipped_density)
      }
      rm(clip2)
    } # end clip


    r <- terra::res(mask)
    if(length(r) == 1) r <- rep(r, 2)
    stopifnot(length(r) == 2)

    p_adj <- .97  # Used to adjust the target number of cells down slightly as
    # we are looking to be below not at max_params

    target_params <- max_params * p_adj # target number of parameters

    n_attempts <- 10
    a_stats <- calc_abundance_stats(abunds,
                                    dummy_dynamic_mask = dummy_dynamic_mask)

    # Iteratively attempt to set resolution
    # there's some inherent slop in the predictions because not all
    # the course cells fully overlap fine cells that have data
    for(i in 1:n_attempts){
      # Calculate target resolution
      f <- function(res) (predict_params(a_stats, res) - target_params)^2
      #o <- optim(par = list(res = a_stats$res), fn = f, method = "Brent",
      #           lower = 1, upper = 1000)
      o <- stats::optimize(f =f, interval = c(1, 1000))

      res <-  o$minimum
      res_m <- 1000 * res



      if(verbose){
        cat("  Attempt ", i , " at setting resolution\n")
        cat("  (", round(res, 3), "km chosen)\n", sep = "")
      }

      # It's still possible to overshoot  - I think because along
      # edges single values at a fine resolution may map to a large cell

      # Trial reprojection
      initial_res <- mean(res(abunds))
      factor <- round(res_m / initial_res)
      if(factor < 1) factor <- 1
      reproject_res <- res_m / factor

      trial_ref <-  terra::project(mask, crs, method = "near", origin = 0,
                                   res = reproject_res)
      trial <- terra::project(abunds, trial_ref)

      if(factor != 1)
        trial <- terra::aggregate(trial,
                                  fact = factor,
                                  fun = mean,
                                  na.rm = TRUE)

      a_stats <- calc_abundance_stats(trial,
                                      dummy_dynamic_mask = dummy_dynamic_mask)

      # Evaluating on actual max_params (not target_params which is adjusted for faster convergence)
      pct_of_target <- a_stats$n_params/ max_params * 100

      if(verbose)
        cat("  ", round(pct_of_target, 2), "% of target (estimate).\n")

      if(pct_of_target <= 100 && pct_of_target > 90){
        if(verbose)
          cat(" success\n")
        break
      } else {
        # Try again (up to 10 times)
        if(verbose)
          cat("  trying again\n")

      }

    } # end resolution trials


    if(pct_of_target > 100 || pct_of_target < 90)
      cat("  Failed to find a resolution that resulted in > 90% and < 100 % of the target parameters.\n")


    # Round
    breaks <-  c(-Inf, 2.5,  5,   100,  300, 600, Inf)  # in km
    precision =   c(0.1,  .5,  1,     2,     5,  10)  # in km
    tp <- precision[findInterval(res, breaks)] # target precision
    res <- ceiling(res / tp)  * tp
    cat("Rounded to", res, "km final resolution.\n")

    if(!missing(clip) && verbose){
      cat("Clipping removed ", format(pct_lost, nsmall = 2), "% of the total density\n", sep = "" )
      rm(pct_lost)
    }


  }  # End if missing res

  if(download_species == "example_data" & res < 30){
    if(verbose)
      cat("Resolution forced to 30 for example data, which only has low resolution images\n")
    res <- 30
  }
  res_m <- 1000 * res # target resolution in meters (res argument uses KM)

  #----------------------------------------------------------------------------#
  # Set output paths  (depends on resolution)                               ####
  #----------------------------------------------------------------------------#
  if(any_output){

    out_base <- file.path(out_dir,
                          paste0(download_species, "_", st_year,"_", res,"km"))

    if(!missing(clip))
      out_base <- paste0(out_base, "_clip")

    paths <- list()
    if(hdf5){
      paths <- c(paths, list( hdf5 = paste0( out_base, ".hdf5" ) ) )
    }
    if(tiff){
      paths <- c(paths, list(
        abundance = paste0(out_base, ".tif"),
        uci = paste0(out_base, "_uci.tif"),
        lci = paste0(out_base, "_lci.tif")
      ))
    }

    exist <- file.exists(unlist(paths))
    if(!overwrite && any(exist)){
      stop("Prexisiting output files: ",
           paste(unlist(paths[exist]), collapse = ", "),
           " Set overwrite = TRUE to overwrite.")
    }
    rm(exist, out_base)
  }

  #----------------------------------------------------------------------------#
  #
  # Process raster data
  #
  #----------------------------------------------------------------------------#

  # Determine which resolution to load
  breaks <-  c(3, 9, 27, Inf)
  levels <- c("hr", "mr", "lr")
  load_res <- levels[findInterval(res, breaks)]


  # load abundance data
  #   uci and lci are short for upper and lower confidence intervals
  if(verbose)
    cat("Reading ",
        switch(load_res,
               "lr" = "low resolution (27 km)",
               "mr"= "medium resolution (9 km)",
               "hr" = "high resolution (3 km)",
               stop("unrecognized resolution")
        ),
        " geoTIFFs\n", sep = "")

  # Download high or medium resolution data (if needed)
  if(load_res != "lr"){
    ebirdst::ebirdst_download(download_species,
                              pattern = download_patterns[[load_res]])
  }
  abunds <- ebirdst::load_raster("abundance", path = sp_path,resolution=load_res)
  abunds_lci <- ebirdst::load_raster("abundance", metric = "lower",
                                     path = sp_path,resolution=load_res)
  abunds_uci <- ebirdst::load_raster("abundance", metric = "upper",
                                     path = sp_path, resolution=load_res)

  if(verbose)
    cat("Creating mask in target resolution and projection\n")

  # Make mask in original coordinate system
  # TRUE if a cell has non-zero data in any layer (timestep) and
  # will be cropped to extent of TRUE cells
  mask  <- make_mask(abunds)

  # Re-project mask and re-crop to extent of data in new projection
  # set output resolution to be a factor of the target resolution so that
  # aggregate can hit target exactly
  initial_res <- mean(res(mask))
  factor <- round(res_m / initial_res)
  if(factor < 1) factor <- 1
  reproject_res <- res_m / factor
  mask<- terra::project(mask, crs, method = "near", origin = 0,
                        res = reproject_res)
  if(!missing(clip)){
    # Note locally mask is a SpatRast that indicates which cells
    # have data (at any timestep). The terra mask function
    #  sets cells to NA if they are outside the polygon.
    mask <- terra::mask(mask, clip)
  }

  mask <- make_mask(mask)  # re-crop to data in new projection

  # Add leading rows and columns to maintain origin of 0 after aggregation
  # (extent / resolution will be an integer in model)
  e <- ext(mask)
  new_ext <- e
  if(!isTRUE(all.equal(e[1] %% res, 0, check.attributes = FALSE))){
    # n_to_add is the number of columns to add on the left
    n_to_add <- factor - round( (e[1] %% res_m) / reproject_res )
    new_ext[1] <- e[1] - n_to_add * reproject_res
  }
  if(!isTRUE(all.equal(e[4] %% res, 0, check.attributes = FALSE))){
    # n_to_add is the number of rows to add at the top
    n_to_add <- factor - round( (as.numeric(e[4]) %% res_m) / reproject_res )
    new_ext[4] <- e[4] + n_to_add * reproject_res
  }
  if(!new_ext == e){ # if extent changed
    mask <- terra::extend(mask, new_ext )
  }

  # Reproject data and crop to mask
  if(verbose)
    cat("Reprojecting and cropping to mask:\n\tabundance")
  abunds <- terra::project(abunds, mask, method = "near")
  if(verbose)
    cat(" done.\n\tUpper CI")
  abunds_uci <- terra::project(abunds_uci, mask, method = "near")
  if(verbose)
    cat(" done.\n\tLower CI")
  abunds_lci <- terra::project(abunds_lci, mask, method = "near")
  if(verbose)
    cat(" done.\n")

  # aggregate to target resolution
  if(factor != 1){
    if(verbose)
      cat("Resampling to target resolution (", res, " km)\n", sep = "")
    abunds_low_res <- terra::aggregate(abunds,
                                       fact = factor,
                                       fun = mean,
                                       na.rm = TRUE)

    abunds_uci_low_res <- terra::aggregate(abunds_uci,
                                           fact = factor,
                                           fun = mean,
                                           na.rm = TRUE)

    abunds_lci_low_res <- terra::aggregate(abunds_lci,
                                           fact = factor,
                                           fun = mean,
                                           na.rm = TRUE)

  } else {

    abunds_low_res <- abunds
    abunds_uci_low_res <- abunds_uci
    abunds_lci_low_res <- abunds_lci

  }
  # Standardize to sum of 1
  # Note we are dividing all three datasets by the total abundance
  # for each timestep (the column sums of the abundance dataset).
  v <- terra::values(abunds_low_res)
  totals <- colSums(v, na.rm = TRUE)
  v_uci <- terra::values(abunds_uci_low_res)
  v_lci <- terra::values(abunds_lci_low_res)
  for(i in seq_len(ncol(v))){
    v[  , i ] <- v[ , i ] / totals[ i ]
    v_uci[ , i ] <- v_uci[ , i ] / totals[ i ]
    v_lci[, i ] <- v_lci[, i ] / totals[ i ]
  }
  terra::values(abunds_low_res) <- v
  terra::values(abunds_uci_low_res) <- v_uci
  terra::values(abunds_lci_low_res)  <- v_lci

  # Double check that there aren't extra cells along edges
  mask <- make_mask(abunds_low_res)
  if(terra::ncell(mask) != terra::ncell(abunds_low_res)){
    abunds_low_res <- terra::crop(abunds_low_res, mask)
    abunds_uci_low_res <- terra::crop(abunds_uci_low_res, mask)
    abunds_lci_low_res <- terra::crop(abunds_lci_low_res, mask)
  }

  #----------------------------------------------------------------------------#
  #  Flatten raster data
  #----------------------------------------------------------------------------#

  # Generate distribution matrix containing the active cells in columns
  # and similar objects for upper and lower confidence intervals
  m <- as.logical(terra::values(mask))
  distr <- terra::values(abunds_low_res)[m, , drop = FALSE]
  distr[is.na(distr)] <- 0
  uci <- terra::values(abunds_uci_low_res)[m , , drop = FALSE]
  uci[is.na(uci)] <- 0
  lci <- terra::values(abunds_lci_low_res)[m , , drop = FALSE]
  lci[is.na(lci)] <- 0

  # Update metadata
  export$metadata$n_active <- n_active <- sum(m)
  export$metadata$n_transitions <- n_transitions <- ncol(distr)


  # Calculate realized number of parameters in BirdFlow model
  n_params <- n_active^2 * (n_transitions) + nrow(distr)
  if(!missing(max_params)){
    pct_max_params <- n_params/max_params*100
  }

  # Append first column onto end so we have full cycle of transitions
  distr <- cbind(distr, distr[, 1, drop = FALSE])
  uci <- cbind(uci, uci[ , 1 , drop = FALSE])
  lci <- cbind(lci, lci[ ,1 ,  drop = FALSE])
  colnames(uci) <- colnames(lci) <-  colnames(distr) <-
    paste0("t", seq_len(ncol(distr)))

  # Save to export object
  export$distr <- distr
  export$uci <- uci
  export$lci <- lci
  export$metadata$has_distr <- TRUE

  #----------------------------------------------------------------------------#
  #  Define geom and metadata                                               ####
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

  # Reformat and export dates
  dates <- as.data.frame(ebirdst::ebirdst_weeks)
  names(dates)[names(dates) == "week_number"] <- "interval"
  dates$doy <- lubridate::yday(dates$date) + 0.5
  dates$date <- as.character(dates$date)

  # Rename ("week_" columns by dropping preffix )
  names(dates) <- gsub("^week_", "", names(dates))

  # add date info for last distribution (a repeat of the first)
  first <- dates[1, , drop = FALSE]
  first$interval <- dates$interval[nrow(dates)] + 1
  dates <- rbind(dates, first)

  # Save to export object
  export$dates <- dates
  export$metadata$n_timesteps <- length(unique(dates$date))

  #----------------------------------------------------------------------------#
  #  Add dynamic_mask and distances                                         ####
  #----------------------------------------------------------------------------#
  export$distances <- great_circle_distances(export) |>
    shorten_distance_matrix()

  export$geom$dynamic_mask <- export$distr > 0

  if(dummy_dynamic_mask)
    export$geom$dynamic_mask[, ] <- TRUE


  #----------------------------------------------------------------------------#
  #  Print details                                                          ####
  #----------------------------------------------------------------------------#

  n_params <- n_parameters(export)

  if(verbose){
    cat("Model has:\n\t",
        sum(m), " active cells,\n\t", n_transitions , " transitions, and\n\t",
        format(n_params, big.mark = ","), " parameters", sep = "")
    if(!missing(max_params)){
      pct_max_params <- n_params / max_params * 100
      cat(", ", round(pct_max_params, 1), "% of maximum parameters\n", sep ="")
    } else {
      cat("\n")
    }
    cat(round(n_params / max_param_per_gb, 1),
        "gb of GPU ram required to fit model.\n"  )

  }




  #----------------------------------------------------------------------------#
  #  Validate                                                            ####
  #----------------------------------------------------------------------------#
  validate_BirdFlow(export, allow_incomplete = TRUE)

  #----------------------------------------------------------------------------#
  #  Write files                                                            ####
  #----------------------------------------------------------------------------#
  if(tiff){

    if(verbose){
      tiff_paths <- paths[!names(paths) == "hdf5"]
      cat("Writing geoTIFFs\n\t")
      cat(paste(names(tiff_paths), ": ",
                unlist(tiff_paths), sep = "", collapse = "\n\t"),
          "\n", sep = "")
      rm(tiff_paths)
    }
    terra::writeRaster(abunds_low_res, paths$abundance, overwrite = overwrite)
    terra::writeRaster(abunds_lci_low_res, paths$lci, overwrite = overwrite)
    terra::writeRaster(abunds_uci_low_res, paths$uci, overwrite = overwrite)
  }

  # Write HDF5
  if(hdf5){
    if(verbose)
      cat("Writing hdf5: ", paths$hdf5, "\n")

    if(file.exists(paths$hdf5) & overwrite ){
      file.remove(paths$hdf5)
    }

    if(file.exists(paths$hdf5))
      stop(paths$hdf5, "already exists and couldn't be deleted.")

    # class(export) <- NULL
    ns <- names(export)
    for(i in seq_along(ns)){
      n <- ns[i]
      rhdf5::h5write(export[[n]],
                     file = paths$hdf5,
                     name = n,
                     native = TRUE,
                     write.attributes = FALSE,
                     createnewfile = i == 1)  # TRUE for first object
    }
  }






  # invisibly return exported BirdFlow model
  invisible(export)
}
