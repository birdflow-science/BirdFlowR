#' Determine BirdFlow model resolution                                     ####
#'
#'  Internal function to determine the resolution to use when creating a
#'  BirdFlow model.  It is called by `preprocess_species()` and nowhere else,
#'  but is complicated enough to justify being it's own function.
#'
#'  When the user specifies a resolution that is the resolution used when
#'  creating the model.
#'
#'  If the `res` argument is `NULL` the heuristic here attempts to set a
#   resolution that will result in close to `max_params` (but stay under it)
#   the total number of fitted parameters in the model. It rounds the
#   resolution variably - rounding more for larger values.
#
#   If `res` and `max_params` are both `NULL` than `max_params` is calculated
#'  from the `gpu_ram` parameter which specifies the the GB of ram available on
#'  the machine used to fit the models.
#'
#'  It turned out to be really hard to anticipate how many cells would
#'  contain data after a resolution change. The code estimates by
#'  calculating the area of the non-zero cells in the current resolution
#'  and then figures out the resolution where the number of cells required
#'  to cover that area matches our target number of parameters. However,
#'  it's a poor estimate because it ignores the fact that coarse cells along
#'  the edges overlap fine cells that contain a mix of no data and data.
#'
#'  The code here makes the estimate applies a correction factor to adjust the
#'  estimate for the bias, resamples to the estimate, evaluates the new
#'  number of non-zero cells (and thus parameters), and repeats until the
#'  estimate converges on a number of parameters between 90 and 100 % of the
#'  target number. This results in a rather precise resolution that can be fit
#'  given the number of parameters which is then rounded up (reducing
#'  parameters) to a cleaner number.
#'
#' @inheritParams preprocess_species
#' @param sp_path The species path used with ebirdst to download and load data
#' @param download_species The species code used with ebirdst this might be
#'   "example_data" but otherwise will be a real species code.
#' @param project_method This is the method used to reproject it is a local
#' variable set within `preprocess_species`.
#'
#' @return The resolution in km either as set directly by the user or as
#' derived from `max_params` or `gpu_ram`.
#'
#' @keywords internal
determine_resolution <- function(sp_path,
                                 res,
                                 max_params,
                                 gpu_ram,
                                 clip,
                                 crs,
                                 download_species,
                                 project_method){


  verbose <- birdflow_options("verbose" )
  max_param_per_gb <- birdflow_options("max_param_per_gpu_gb")

  if (!is.null(res)) {
    return(res)
  }

  if (is.null(max_params)) {
    stopifnot(is.numeric(gpu_ram),
              length(gpu_ram) == 1,
              !is.na(gpu_ram),
              gpu_ram > 0)
    max_params <- max_param_per_gb * gpu_ram
    if (verbose)
      cat("Setting max_params to ", max_params, " anticipating ",
          gpu_ram, " GB of GPU ram.\n")
  }

  if (verbose)
    cat("Calculating resolution\n")
  # Load low res abundance data and calculate total areas birds occupy at any
  # time (active_sq_m)
  abunds <- ebirdst::load_raster("abundance",
                                 path = sp_path, resolution = "lr")

  # Treat NA values as zeros - this better reflects what they actually are
  v <- terra::values(abunds)
  v[is.na(v)] <- 0
  terra::values(abunds) <- v

  mask <- make_mask(x = abunds, assume_no_na = TRUE)

  if (!is.null(clip)) {
    clip2 <- terra::project(clip, terra::crs(mask))
    mask <- terra::mask(mask, clip2)
    mask[is.na(mask)] <- FALSE
    abunds <- terra::mask(abunds, clip2)




    if (verbose) {
      # Calculate percent of density lost
      # will print after printing the resolved resolution
      sa <- sum(abunds)
      csa <- terra::mask(sa, clip2)
      tot_density <- sum(terra::values(sa), na.rm = TRUE)
      clipped_density <- sum(terra::values(csa), na.rm = TRUE)
      pct_lost <-
        round((tot_density - clipped_density) / tot_density * 100, 2)
      rm(sa, csa, tot_density, clipped_density)
    }
    rm(clip2)
  } # end clip


  r <- terra::res(mask)
  if (length(r) == 1)
    r <- rep(r, 2)
  stopifnot(length(r) == 2)

  p_adj <- .97  # Used to adjust the target number of cells down slightly as
  # we are looking to be below not at max_params

  target_params <- max_params * p_adj # target number of parameters

  n_attempts <- 10
  a_stats <- calc_abundance_stats(abunds)

  # Iteratively attempt to set resolution
  # there's some inherent slop in the predictions because not all
  # the coarse cells fully overlap fine cells that have data
  for (i in 1:n_attempts) {
    # Calculate target resolution
    f <- function(res) (predict_params(a_stats, res) - target_params)^2
    o <- stats::optimize(f = f, interval = c(1, 1000))

    res <-  o$minimum
    res_m <- 1000 * res

    if (verbose) {
      cat("  Attempt ", i, " at setting resolution\n")
      cat("  (", round(res, 3), "km chosen)\n", sep = "")
    }

    # Trial transformation
    initial_res <- mean(res(abunds))
    factor <- round(res_m / initial_res)
    if (factor < 1)
      factor <- 1
    reproject_res <- res_m / factor
    trial_ref <-  terra::project(mask, crs, method = project_method,
                                 origin = 0, res = reproject_res)
    trial <- terra::project(abunds, trial_ref, method = project_method)

    if (factor != 1) {
      trial <- terra::aggregate(trial,
                                fact = factor,
                                fun = mean,
                                na.rm = TRUE)
    }

    a_stats <- calc_abundance_stats(trial)

    # Evaluating on actual max_params
    #  - not target_params which is slightly lower
    pct_of_target <- a_stats$n_params / max_params * 100
    if (verbose)
      cat("  ", round(pct_of_target, 2), "% of target (estimate).\n")

    if (pct_of_target <= 100 && pct_of_target > 90) {
      if (verbose)
        cat(" success\n")
      break
    } else {
      # Try again (up to 10 times)
      if (verbose)
        cat("  trying again\n")
    }
  } # end resolution trials

  if (pct_of_target > 100 || pct_of_target < 90)
    cat("  Failed to find a resolution that resulted in > 90% and < 100 % of",
        "the target parameters.\n")

  # Round
  breaks <-  c(-Inf, 2.5,  5,  300, 600, Inf)  # in km
  precision <- c(0.1,  .5,  1,    5,  10)  # in km
  tp <- precision[findInterval(res, breaks)] # target precision
  res <- ceiling(res / tp)  * tp
  if (verbose)
    cat("Rounded to", res, "km final resolution.\n")

  if (!is.null(clip) && verbose) {
    cat("Clipping removed ", format(pct_lost, nsmall = 2),
        "% of the total density\n", sep = "")
    rm(pct_lost)
  }

  # With example date force resolution to be at least 30
  if (download_species == "example_data" && res < 30) {
    if (verbose)
      cat("Resolution forced to 30 for example data,",
          "which only has low resolution images\n")
    res <- 30
  }

  return(res)

}
