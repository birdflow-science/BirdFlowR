# species <- "amewoo"
# res <- 80

#' download and format eBird Status and Trends data for BirdFlow model fitting
#'
#' This uses the \pkg{ebirdst} package to download crop and resample abundance
#' data for the given species. It then writes the abundance data along with
#' a mask, spatial reference information, and species metadata to an HDF5 file.
#'
#' @param species a species in any format accepted by [ebirdst::get_species()]
#' @param res the target resolution of the BirdFlow model in kilometers.
#' @param p the proportion of the species distribution to be retained
#' (within each timestep), must be greater than 0 and less than or equal to 1.
#' In practice it should be above 0.9 and will probably be above 0.99. This
#' controls how much (if any) of the tail of the distribution density is
#' trimmed from the model.  1 indicates no trimming.
#' @param out_dir output directory, files will be written here
#' @param tiff if TRUE geoTIFF files will be exported
#' @param overwrite if TRUE any pre-existing output files will be overwritten,
#' otherwise the presence of pre-existing files is an error.
#' @param crs coordinate reference system to use.  Defaults to [birdflow_crs].
#' @importFrom ebirdst get_species
#' @return this function invisibly returns a list of the objects that are
#'  written to the hdf5 file.
#' @export
#'
#' @examples
preprocess_species <- function(species, res = 80, p = 1, out_dir,
                               tiff = FALSE,
                               overwrite = FALSE,
                               crs = birdflow_crs){

  # Validate inputs
  stopifnot(length(p) == 1, p > 0, p <= 1)
  if(length(species) != 1)
    stop("Can only preprpocess one species at a time")
  species <- ebirdst::get_species(species)
  stopifnot(length(res) == 1)
  if (res < 3) {
    stop("Resolution cannot be less than 3 km")
  }

  # Define local variables
  res_m <- 1000 * res # target resolution in meters (res argument uses KM)
  st_year <- ebirdst::ebirdst_version()$version_year

  # Define an object to hold everything that we are going to export
  # structured similar to a BirdFlow object
  export <- new_BirdFlow()
  export$trans <- NULL
  export$marginals <- NULL
  class(export) <- NULL

  # Set output paths
  out_dir <- gsub("/$|\\\\$", "", out_dir) # drop trailing slash
  if(!dir.exists(out_dir))
     stop("output directory ", out_dir, " does not exist.")
  out_base <- file.path(out_dir,
                             paste0(species, "_", st_year,"_", res,"km"))

  if(p != 1)
    out_base <- paste0(out_base, "_p", 100 *p)

  paths <- list(hdf5 = paste0(out_base, ".hdf5"))
  if(tiff){
    paths <- c(paths, list(
      abunds = paste0(out_base, ".tif"),
      se = paste0(out_base, "_se.tif"),
      quantiles = paste0(out_base, "_quantiles.tif")
    ))
  }

  exist <- file.exists(unlist(paths))
  if(!overwrite && any(exist)){
    stop("Prexisiting output files: ",
         paste(unlist(paths[exist]), collapse = ", "),
         " Set overwrite = TRUE to overwrite.")
  }
  rm(exist, out_base)

  #----------------------------------------------------------------------------#
  # format species metadata (spmd)                                          ####
  #----------------------------------------------------------------------------#
  er <- ebirdst::ebirdst_runs
  spmd <- as.list(er[er$species_code == species, , drop = FALSE])
  cat("Species resolved to: '", species, "' (", spmd$common_name, ")\n", sep ="")

  # Reformat dates as strings
  date_to_char <- function(x){
    if(class(x) == "Date")
      x <- as.character(x)
    return(x)
  }
  spmd <- lapply(spmd, date_to_char)
  export$spmd <- spmd

  # Add ebirdst verions to  metadata
  v <- ebirdst::ebirdst_version()
  export$metadata$ebird_version_year <- v$version_year
  export$metadata$ebird_release_year <- v$release_year
  export$metadata$ebird_access_end_date <- as.character(v$access_end_date)
  export$metadata$birdflow_preprocess_date <- as.character(Sys.Date())


  #----------------------------------------------------------------------------#
  # Download and format abundance data                                      ####
  #----------------------------------------------------------------------------#

  # Download - requires setting code with set_ebirdst_access_key()
  # Saves to disk at a standard path within user's home directory
  # For windows 10 was:
  #  ~\AppData\Roaming\R\data\R\ebirdst\2021\[species]
  # logical below attempts to supress messages if the data has already been
  # downloaded.
  if(file.exists(ebirdst::get_species_path(species))){
    sp_path <-  suppressMessages(ebirdst::ebirdst_download(species))
  } else {
    sp_path <-  ebirdst::ebirdst_download(species)
  }
  # Determine which resolution to load
  breaks <-  c(3, 9, 27, Inf)
  levels <- c("hr", "mr", "lr")
  load_res <- levels[findInterval(res, breaks)]

  # load abundance data
  abunds <- ebirdst::load_raster("abundance", path = sp_path,resolution=load_res)
  abunds_lo <- ebirdst::load_raster("abundance", metric = "lower",
                           path = sp_path,resolution=load_res)
  abunds_hi <- ebirdst::load_raster("abundance", metric = "upper",
                           path = sp_path, resolution=load_res)

  dates <- ebirdst::parse_raster_dates(abunds)
  dates <- as.character(dates)

  # Convert to terra::rast
  abunds <- terra::rast(abunds)
  abunds_lo <- terra::rast(abunds_lo)
  abunds_hi <- terra::rast(abunds_hi)

  # Make mask in original coordinate system
  # TRUE if a cell has non-zero data in any layer (timestep) and
  # will be cropped to extent of TRUE cells
  mask  <- make_mask(abunds)

  # Re-project mask and re-crop to extent of data in new projection
  # Force the output to align to origin.
  # set output resolution to be a factor of the target resolution so that
  # aggregate can hit target exactly
  initial_res <- mean(res(mask))
  factor <- round(res_m / initial_res)
  reproject_res <- res_m / factor
  mask<- project(mask, crs(mollweide_wkt), method = "near", origin = 0,
                 res = reproject_res)
  mask <- make_mask(mask)  # re-crop to data in new projection

  # Reproject data and crop to mask
  abunds <- terra::project(abunds, mask, method = "near")
  abunds_hi <- terra::project(abunds_hi, mask, method = "near")
  abunds_lo <- terra::project(abunds_lo, mask, method = "near")

  # Calculate standard error
  # This was copied from Benjamin's code
  # the confidence intervals are the 10th and 90th quantiles.
  abunds_se <-  (abunds_hi - abunds_lo)/4

  rm(abunds_hi, abunds_lo) # high and low CI - only used for SE


  # aggregate to target resolution
  abunds_low_res <- aggregate(abunds,
                              fact = factor,
                              fun = mean,
                              na.rm = TRUE)

  abunds_se_low_res <- aggregate(abunds_se,
                                 fact = factor,
                                 fun = mean,
                                 na.rm = TRUE)

  # Renormalize
  v <- values(abunds_low_res)
  totals <- colSums(v, na.rm = TRUE)
  v_se <- values(abunds_se_low_res)
  for(i in seq_len(ncol(v))){
    v[  , i ] <- v[ , i ] / totals[ i ]
    v_se[ , i ] <- v_se[ , i ] / totals[ i ]
  }
  values(abunds_low_res) <- v
  values(abunds_se_low_res) <- v_se

  # Double check that there aren't extra cells along edges
  mask <- make_mask(abunds_low_res)
  if(ncell(mask) != ncell(abunds_low_res)){
    abunds_low_res <- crop(abunds_low_res, mask)
    abunds_se_low_res <- crop(abunds_se_low_res, mask)
  }

  #----------------------------------------------------------------------------#
  #  Trim distribution                                                      ####
  #----------------------------------------------------------------------------#
  # This trims low values off the end of the distribution to create a more
  # parsimonious dataset.
  # the Argument p defines the proportion of the total density we want to
  # retain

  if(p != 1){
    cat("Trimming tail of distribution. Retaining ",
        p * 100 , "% of the density.\n")
    # Force to zero the values that are below the threshold
    v <- values(abunds_low_res)
    thresholds <- apply(v, 2, find_threshold, p = p)
    for(i in seq_len(ncol(v))){
      d <- v[ , i]
      d[d < thresholds[i]] <- 0
      v[ , i] <- d
    }

    # Renormalize
    #  divide SE by same totals as in abunds to keep them in sync
    totals <- colSums(v, na.rm = TRUE)
    v_se <- values(abunds_se_low_res)
    for(i in seq_len(ncol(v))){
      v[  , i ] <- v[ , i ] / totals[ i ]
      v_se[ , i ] <- v_se[ , i ] / totals[ i ]
    }
    values(abunds_low_res) <- v
    values(abunds_se_low_res) <- v_se

    # Because we've forced some values to zero can now remask and recrop
    new_mask <- make_mask(abunds_low_res)
    cat("Trimming reduced extent from ", sum(values(mask)),  "/", ncell(mask),
        " to ", sum(values(new_mask)) , "/", ncell(new_mask),
        "(active cells/extent cells).", sep = "")
    mask <- new_mask
    abunds_low_res <- crop(abunds_low_res, mask)
    abunds_se_low_res <- crop(abunds_low_res, mask)
  }

  #### NEED to calculate quantiles
  ###  code from Benjamin's function:
  #  abunds_quantiles  <- setValues(abunds_low_res.moll,
  # apply(getValues(abunds_low_res.moll),2,function(x) {
  #  ecdf(x[x>0])(x)
  #  }))


  # Generate distribution matrix (selected cells in columns)
  m <- as.logical(values(mask))
  distr <- values(abunds_low_res)[m, , drop = FALSE]
  distr[is.na(distr)] <- 0

  # Append first column onto end so we have full cycle of transitions
  distr <- cbind(distr, distr[, 1, drop = FALSE])
  colnames(distr) <- paste0("t", seq_len(ncol(distr)))
  dates <- c(dates, dates[1])   # updates dates to match
  export$distr <- distr

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
  m <- values(mask)
  m <- matrix(m, nrow = nrow(mask), ncol = ncol(mask), byrow = TRUE)
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
  #w <- ebirdst_weeks$date
  # lags <- w[-1] - w[-(length(w))]  # ebird has an 8 day week too!
  #----------------------------------------------------------------------------#
  #  Write files                                                            ####
  #----------------------------------------------------------------------------#


  if(tiff){
    terra::writeRaster(abunds_low_res, paths$abunds)
    terra::writeRaster(abunds_se, paths$se)
    #### QUANTILES too!

  }


  ##### Write HDF5

  invisible(export)
}
