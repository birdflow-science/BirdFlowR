#' process_rasters
#'
#' Internal function to process rasters from eBird Status and Trends
#' for use with a BirdFlow model. Called only from `preprocess_species()` but
#' sufficiently complicated to justify being a separate function.
#'
#' @param res Output resolution in kilometers
#' @param download_species The species code used when downloading eBird
#' S&T data, might be "example_data" but otherwise a standard species code.
#' @param sp_path The path used when downloading the species data - passed to
#' \pkg{ebirdst} functions.
#' @param clip polygon indicating the area to process or NULL to process entire
#' species range.
#' @param project_method Method to use when reprojecting. Set locally by code
#' within `preprocess_species()`
#' @inheritParams preprocess_species
#' @return A list with
#' \item{distr, uci, lci}{The are the species distribution, and upper and lower
#' confidence intervals on that distribution in their flattened form. Each
#' timestep is stored in a column with values for unmasked cells only.}
#' \item{m}{The mask stored as a logical matrix with TRUE representing active
#' cells in the model}
#' \item{mask}{The mask stored as a terra::SpatRaster}
#' @keywords internal
process_rasters <- function(res,
                            crs,
                            download_species,
                            sp_path,
                            clip,
                            project_method,
                            download_patterns,
                            trim_quantile = NULL) {


  res_m <- res * 1000


  bf_msg("process_rasters called with download_species =",
         download_species, "\n")


  #----------------------------------------------------------------------------#
  # Load abundance data
  #----------------------------------------------------------------------------#

  # Determine which resolution to load
  breaks <- c(3, 9, 27, Inf)
  levels <- c("hr", "mr", "lr")
  load_res <- levels[findInterval(res, breaks)]

  bf_msg("Reading ",
         switch(load_res,
                "lr" = "low resolution (27 km)",
                "mr" = "medium resolution (9 km)",
                "hr" = "high resolution (3 km)",
                stop("unrecognized resolution")),
         " geoTIFFs\n")

  # Download high or medium resolution data (if needed)
  bf_suppress_msg({
    if (load_res != "lr") {
      if (ebirdst_pkg_ver() < "3.2022.0") {
        ebirdst::ebirdst_download(
          download_species,
          pattern = download_patterns[[load_res]])
      } else {
        cat(download_species, "\n", download_patterns[[load_res]], "\n")
        ebirdst::ebirdst_download_status(
          download_species,
          pattern = download_patterns[[load_res]])
      }
    }
  })
  # Read abundance and upper and lower confidence intervals
  if (ebirdst_pkg_ver() < "3.2022.0") {
    abunds <- ebirdst::load_raster("abundance",
                                   path = sp_path,
                                   resolution = load_res)
    abunds_lci <- ebirdst::load_raster("abundance",
                                       metric = "lower",
                                       path = sp_path,
                                       resolution = load_res)
    abunds_uci <- ebirdst::load_raster("abundance",
                                       metric = "upper",
                                       path = sp_path,
                                       resolution = load_res)
  } else {
    abunds <- ebirdst::load_raster(species = download_species,
                                   product = "abundance",
                                   resolution = res_label(load_res))

    abunds_lci <- ebirdst::load_raster(species = download_species,
                                       product = "abundance",
                                       metric = "lower",
                                       resolution = res_label(load_res))
    abunds_uci <- ebirdst::load_raster(species = download_species,
                                       product = "abundance",
                                       metric = "upper",
                                       resolution = res_label(load_res))
  }

  # Overwrite NA with zero
  # More often than not the NA represents very different habitat
  # than nearby non-na values so is usually more appropriately t
  # treated as non-habitat (zero) than as unkown.
  for (name in c("abunds", "abunds_lci", "abunds_uci")) {
    r <- get(name)
    v <- terra::values(r)
    v[is.na(v)] <- 0
    terra::values(r) <- v
    assign(name, r)
  }


  bf_msg("Creating mask in target resolution and projection\n")

  # Make mask in original coordinate system
  # TRUE if a cell has non-zero data in any layer (timestep) and
  # will be cropped to extent of TRUE cells
  mask <- make_mask(abunds, assume_no_na = TRUE)

  # Re-project mask and re-crop to extent of data in new projection
  # set output resolution to be a factor of the target resolution so that
  # aggregate can hit target exactly
  initial_res <- mean(res(mask))
  factor <- round(res_m / initial_res)
  if (factor < 1)
    factor <- 1
  reproject_res <- res_m / factor

  mask <- terra::project(mask, crs, method = project_method, origin = 0,
                         res = reproject_res)
  if (!is.null(clip)) {
    # Note locally mask is a SpatRast that indicates which cells
    # have data (at any timestep). The terra mask function
    #  sets cells to NA if they are outside the polygon.
    mask <- terra::mask(mask, clip)
  }

  mask <- make_mask(mask)  # re-crop to data in new projection

  # Add leading rows and columns to maintain origin of 0, 0 after aggregation.
  #  extent / resolution will be an integer in model.
  e <- ext(mask)
  new_ext <- e
  if (!isTRUE(all.equal(e[1] %% res_m, 0, check.attributes = FALSE))) {
    # n_to_add is the number of columns to add on the left
    n_to_add <- round((e[1] %% res_m) / reproject_res)
    new_ext[1] <- e[1] - n_to_add * reproject_res
  }
  if (!isTRUE(all.equal(e[4] %% res_m, 0, check.attributes = FALSE))) {
    # n_to_add is the number of rows to add at the top
    n_to_add <- factor - round((as.numeric(e[4]) %% res_m) / reproject_res)
    new_ext[4] <- e[4] + n_to_add * reproject_res
  }
  if (!new_ext == e) { # if extent changed
    mask <- terra::extend(mask, new_ext)
  }

  # Reproject data and crop to mask
  bf_msg("Reprojecting and cropping to mask:\n\tabundance")
  abunds <- terra::project(abunds, mask, method = project_method)
  bf_msg(" done.\n\tUpper CI")
  abunds_uci <- terra::project(abunds_uci, mask, method =  project_method)
  bf_msg(" done.\n\tLower CI")
  abunds_lci <- terra::project(abunds_lci, mask, method =  project_method)
  bf_msg(" done.\n")

  # Clip data
  if (!is.null(clip)) {
    abunds <- terra::mask(abunds, clip)
    abunds_uci <- terra::mask(abunds_uci, clip)
    abunds_lci <- terra::mask(abunds_lci, clip)
  }

  # aggregate to target resolution
  if (factor != 1) {
    bf_msg("Resampling to target resolution (", res, " km)\n")
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
  for (i in seq_len(ncol(v))) {
    v[, i] <- v[, i] / totals[i]
    v_uci[, i] <- v_uci[, i] / totals[i]
    v_lci[, i] <- v_lci[, i] / totals[i]
  }
  terra::values(abunds_low_res) <- v
  terra::values(abunds_uci_low_res) <- v_uci
  terra::values(abunds_lci_low_res)  <- v_lci

  # Double check that there aren't extra cells along edges
  mask <- make_mask(abunds_low_res)
  if (terra::ncell(mask) != terra::ncell(abunds_low_res)) {
    abunds_low_res <- terra::crop(abunds_low_res, mask)
    abunds_uci_low_res <- terra::crop(abunds_uci_low_res, mask)
    abunds_lci_low_res <- terra::crop(abunds_lci_low_res, mask)
  }

  #----------------------------------------------------------------------------#
  #  Flatten raster data (limit to active cells)                            ####
  #----------------------------------------------------------------------------#

  # Generate distribution matrix containing the active cells in columns
  # and similar objects for upper and lower confidence intervals
  m <- as.logical(terra::values(mask))
  distr <- terra::values(abunds_low_res)[m, , drop = FALSE]
  distr[is.na(distr)] <- 0
  uci <- terra::values(abunds_uci_low_res)[m, , drop = FALSE]
  uci[is.na(uci)] <- 0
  lci <- terra::values(abunds_lci_low_res)[m, , drop = FALSE]
  lci[is.na(lci)] <- 0

  #----------------------------------------------------------------------------#
  # Trim outliers
  #----------------------------------------------------------------------------#
  if (!is.null(trim_quantile)) {

    nt <- ncol(distr)

    stopifnot(is.numeric(trim_quantile),
              !anyNA(trim_quantile),
              all(trim_quantile > 0),
              all(trim_quantile <= 1),
              length(trim_quantile) == 1 || length(trim_quantile) == nt)

    if (length(trim_quantile) == 1)
      trim_quantile <- rep(trim_quantile, nt)

    truncate_and_renormalize <- function(x, p = 0.95) {
      q <- quantile(x[!x == 0], p = p)
      x[x > q] <- q
      x <- x / sum(x, na.rm = TRUE)
      x
    }

    for (i in seq_len(ncol(distr))) {
      distr[, i] <- truncate_and_renormalize(distr[, i], p = trim_quantile[i])
    }
  }  # end trim outliers


  return(list(distr = distr,
              uci = uci,
              lci = lci,
              m = m,
              mask = mask))

}
