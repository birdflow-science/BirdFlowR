#' prepare eBird Status and Trends data for BirdFlow model fitting
#'
#' Write a template BirdFlow object to an hdf5 file based on distribution data
#' downloaded with \pkg{ebirdst}. The object is complete except for
#'  marginals and transitions.
#'
#' @param species a species in any format accepted by [ebirdst::get_species()]
#' @param out_dir output directory, files will be written here. Required unless
#' both `tiff` and `hdf5` are TRUE.  File names created here will
#' incorporate the species code, resolution, and eBird version year.
#' @param res the target resolution of the BirdFlow model in kilometers if
#' omitted a resolution will be chosen that results slightly less than
#' `max_params` parameters.
#' @param hdf5 if TRUE (default) an hdf5 file will be exported.
#' @param tiff if TRUE (default) geoTIFF files will be exported.
#' @param overwrite if TRUE (default) any pre-existing output files will
#' be overwritten. If FALSE pre-existing files will result in an error.
#' @param crs coordinate reference system (CRS) to use.  Defaults to the custom
#' projection eBird has assigned to this species - see
#' [ebirdst::load_fac_map_parameters()]). It will be interpreted by
#' [terra::crs()] to generate a well known text representation of the CRS.
#' @param max_params the maximum number of fitted parameters that the BirdFlow
#'  model should contain. If `res` is omitted a resolution will be chosen that
#'  yields this many fitted parameters. The default value represents the number
#'  of parameters that could efficiently be fit in early BirdFlow models
#'  (around 4000 active cells and 51 transitions).
#' @param p Set to values less than 1 to trim the tail off of the distributions.
#' `p` is the proportion of the species distribution to be retained (within each
#' timestep). In practice it should be between 0.95 (or even 0.99) and 1.
#' The default of 1 indicates no trimming. This parameter (and functionality)
#' may be dropped as eBird status and trends data has already been
#' trimmed. The heuristic to determine resolution does not factor in
#' trimming.
#' @importFrom ebirdst get_species
#' @return returns a BirdFlow model object that lacks
#' marginals, but is otherwise complete.
#' @export
#'
#' @examples
#' \dontrun{
#'  dir <- tempdir()
#'  bf <- preprocess_species("amewoo", dir )
#'
#'  plot(rasterize_distr(get_distr(c(1, 26), bf), bf))
#'  unlink(dir)
#'
#' }
#'
preprocess_species <- function(species,
                               out_dir,
                               res,
                               hdf5 = TRUE,
                               tiff = TRUE,
                               overwrite = TRUE,
                               crs,
                               max_params = 8.1e+8,
                               p = 1){

  # Validate inputs
  stopifnot(length(p) == 1, p > 0, p <= 1)
  if(length(species) != 1)
    stop("Can only preprpocess one species at a time")
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
  verbose <- TRUE
  any_output <- hdf5 || tiff

  export <- new_BirdFlow()
  export$trans <- NULL
  export$marginals <- NULL

  #----------------------------------------------------------------------------#
  # format species metadata (bf$species)                                          ####
  #----------------------------------------------------------------------------#
  er <- ebirdst::ebirdst_runs
  spmd <- as.list(er[er$species_code == species, , drop = FALSE])

  if(verbose)
    cat("Species resolved to: '", species, "' (", spmd$common_name, ")\n", sep ="")

  # Reformat dates as strings
  date_to_char <- function(x){
    if(class(x) == "Date")
      x <- as.character(x)
    return(x)
  }
  spmd <- lapply(spmd, date_to_char)

  # Check that ebirdst species data supports BirdFlow modelling
  if(spmd$resident)
    stop(spmd$common_name, " (", spmd$species_code, ") is a resident ",
         "(non-migratory) species and is therefore a poor candidate for ",
         "BirdFlow modeling.")

  model_coverage_variables <- c("breeding_range_modeled",
                                "nonbreeding_range_modeled",
                                "postbreeding_migration_range_modeled",
                                "prebreeding_migration_range_modeled")
  if(! all( unlist( spmd[model_coverage_variables]) ) )
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
  if(file.exists(ebirdst::get_species_path(download_species))){
    sp_path <-  suppressMessages(ebirdst::ebirdst_download(download_species))
  } else {
    sp_path <-  ebirdst::ebirdst_download(download_species)
  }

  #----------------------------------------------------------------------------#
  # Determine BirdFlow model resolution                                                    ####
  #   If the res argument isn't supplied the heuristic here attempts to set a
  #   resolution that will result in close to max_params (but stay under it)
  #   the total number of fitted parameters in the model. It rounds the
  #   resolution variably - rounding more for larger values.
  #----------------------------------------------------------------------------#

  if(missing(res)){
    if(download_species == "example_data"){
      if(verbose)
        cat("Resolution forced to 30 for example data.\n")
      res <- 30
    } else {

      if(verbose)
        cat("Calculating resolution")
      # Load low res abundance data and calculate total areas birds occupy at any
      # time (active_sq_m)
      abunds <- terra::rast(ebirdst::load_raster("abundance",
                                                 path = sp_path,
                                                 resolution="lr"))
      mask <- make_mask(x = abunds)
      r <- terra::res(mask)
      stopifnot(length(r) == 2)
      active_sq_m <- sum(terra::values(mask)) * prod(r)

      # Calculate target resolution
      target_cells <- sqrt(max_params / 52)  # target number of cells
      target_res <- sqrt(active_sq_m / target_cells ) # target resolution (meters)
      target_res_km <- round(target_res / 1000)

      # Variably round resolution
      #   Precision used in rounding for each interval defined in breaks
      breaks <-  c(-Inf, 5, 10, 20, 100, 250, Inf)  # in km
      precision = c(0.5, 1, 5, 10, 50, 100)  # in km
      tp <- precision[findInterval(target_res_km, breaks)] # target precision
      res <- ceiling(target_res_km / tp)  * tp
      # round() would be closer, but might overshoot
      cat(" (", res, "km chosen)\n", sep = "")
    }
    res_m <- 1000 * res # target resolution in meters (res argument uses KM)

    #----------------------------------------------------------------------------#
    # Set output paths  (depends on resolution)                               ####
    #----------------------------------------------------------------------------#
    if(any_output){
      out_dir <- gsub("/$|\\\\$", "", out_dir) # drop trailing slash
      if(!dir.exists(out_dir))
        stop("output directory ", out_dir, " does not exist.")
      out_base <- file.path(out_dir,
                            paste0(download_species, "_", st_year,"_", res,"km"))
      if(p != 1)
        out_base <- paste0(out_base, "_p", 100 *p)

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
  }  # end if(missing(res))

  #----------------------------------------------------------------------------#
  #
  # Process raster data
  #
  #----------------------------------------------------------------------------#

  # Determine which resolution to load
  breaks <-  c(3, 9, 27, Inf)
  levels <- c("hr", "mr", "lr")
  load_res <- levels[findInterval(res, breaks)]

  # Load map parameters and set crs
  mp  <- ebirdst::load_fac_map_parameters(path = sp_path)
  if(missing(crs)){
    crs <- terra::crs(mp$custom_projection)
  } else {
    crs <- terra::crs(crs)
  }

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
  abunds <- ebirdst::load_raster("abundance", path = sp_path,resolution=load_res)
  abunds_lci <- ebirdst::load_raster("abundance", metric = "lower",
                                     path = sp_path,resolution=load_res)
  abunds_uci <- ebirdst::load_raster("abundance", metric = "upper",
                                     path = sp_path, resolution=load_res)

  # Convert to terra::rast
  abunds <- terra::rast(abunds)
  abunds_lci <- terra::rast(abunds_lci)
  abunds_uci <- terra::rast(abunds_uci)

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
  reproject_res <- res_m / factor
  mask<- terra::project(mask, crs, method = "near", origin = 0,
                        res = reproject_res)
  mask <- make_mask(mask)  # re-crop to data in new projection

  # Add leading rows and columns to maintain origin of 0 after aggregation
  # (extent / resolution will be an integer in model)
  e <- ext(mask)
  new_ext <- e
  if(!isTRUE(all.equal(e[1] %% res, 0, check.attributes = FALSE))){
    # n_to_add is the number of columns to add on the left
    n_to_add <- factor - round( (e[1] %% res_m) / reproject_res )
    new_ext[1] <- ext[1] - n_to_add * reproject_res
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


  # Renormalize
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
  #  Trim distribution                                                      ####
  #----------------------------------------------------------------------------#
  # This trims low values off the end of the distribution to create a more
  # parsimonious dataset.
  #
  # the Argument p defines the proportion of the total density we want to
  # retain.
  #
  # I may drop this section entirely as it seems like eBird folks have
  #  eliminated long tails at a sensible cut point already.
  #
  if(p != 1){
    cat("Trimming tail of distribution. Retaining ",
        p * 100 , "% of the density.\n")
    # Force to zero the values that are below the threshold
    v <- terra::values(abunds_low_res)
    thresholds <- apply(v, 2, find_threshold, p = p)
    for(i in seq_len(ncol(v))){
      d <- v[ , i]
      d[d < thresholds[i]] <- 0
      v[ , i] <- d
    }

    # Renormalize
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

    # Because we've forced some values to zero can now remask and recrop
    new_mask <- make_mask(abunds_low_res)
    cat("Trimming reduced extent from ", sum(terra::values(mask)),  "/", ncell(mask),
        " to ", sum(terra::values(new_mask)) , "/", ncell(new_mask),
        "(active cells/extent cells).", sep = "")
    mask <- new_mask
    abunds_low_res <- crop(abunds_low_res, mask)
    abunds_lci_low_res <- crop(abunds_lci_low_res, mask)
    abunds_uci_low_res <- crop(abunds_uci_low_res, mask)
  } # Done trimming distribution (if p != 1)

  #----------------------------------------------------------------------------#
  #  Flatten raster data
  #----------------------------------------------------------------------------#

  # Generate distribution matrix (selected cells in columns)
  # and similar objects for upper and lower confidence intervals
  m <- as.logical(terra::values(mask))
  distr <- terra::values(abunds_low_res)[m, , drop = FALSE]
  distr[is.na(distr)] <- 0
  uci <- terra::values(abunds_uci_low_res)[m , , drop = FALSE]
  uci[is.na(uci)] <- 0
  lci <- terra::values(abunds_lci_low_res)[m , , drop = FALSE]
  lci[is.na(lci)] <- 0

  # Calculate realized number of parameters in BirdFlow model
  n_active <- sum(m)
  n_transitions <- ncol(distr) - 1
  n_params <- n_active^2 * (n_transitions) + nrow(distr)
  pct_max_params <- n_params/max_params*100
  if(verbose)
    cat("Model has:\n\t",
        sum(m), " active cells,\n\t", ncol(distr) - 1, " transitions, and\n\t",
        format(n_params, big.mark = ","), " parameters (",
        round(pct_max_params, 1), "% of maximum parameters)\n",
        sep ="")

  # Append first column onto end so we have full cycle of transitions
  distr <- cbind(distr, distr[, 1, drop = FALSE])
  uci <- cbind(uci, uci[ , 1 , drop = FALSE])
  lci <- cbind(lci, lci[ ,1 ,  drop = FALSE])
  colnames(uci) <- colnames(lci) <-  colnames(distr) <-
    paste0("t", seq_len(ncol(distr)))
  export$distr <- distr
  export$uci <- uci
  export$lci <- lci

  export$metadata$has_distr <- TRUE
  export$metadata$n_timesteps <- ncol(distr) - 1 # in distr last is a repeat of 1st

  #----------------------------------------------------------------------------#
  #  Define geom  and metadata                                              ####
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

  # add date info for last distribution (a repeat of the first)
  first <- dates[1, , drop = FALSE]
  first$interval <- dates$interval[nrow(dates)] + 1
  dates <- rbind(dates, first)

  export$dates <- dates

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
  #  class(export) <- "BirdFlow"

  # invisibly return exported BirdFlow model
  invisible(export)
}
