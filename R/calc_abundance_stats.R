
#' Calculate stats from a spatRaster with abundances for each timestep
#'
#' Calculate the total number of parameters and the occupied area for each
#' timestep from an abundance raster.
#'
#' This is a helper to [preprocess_species()]
#'
#' @param x a multilayer spatRaster with relative abundances for each timestep
#' @param circular if TRUE (the default) use the first timestep abundance as the
#'   ending abundance.
#' @return A list with
#' \item{n_params}{The number of parameters there would be in the model if fit
#' on `x`}
#'   \item{count}{ a vector of the number of cells occupied at each timestep}
#'   \item{area}{ a vector of area (sq m) that is in included cells for each
#'   timestep} \item{res}{ the resolution of the raster in km}
#' @keywords internal
calc_abundance_stats <- function(x, circular = TRUE){

  m <- terra::values(x, mat = TRUE)
  m[ is.na(m)] <- 0
  m <-  as.logical(m)
  a <- array(m, dim = dim(x)) # dims:  x, y, timestep/week
  # counts is the number of unmasked cells for each timestep

  cts <- apply(a, 3, sum)


  if(circular)
    cts <- c(cts, cts[1])

  return(list(n_params = sum(cts[-length(cts)] * cts[-1], cts[1]),
              count = cts,
              area = cts * xres(x) * yres(x),  # sq m
              res = mean(xres(x), yres(x)) / 1000 )) # km
}



#' predict the number of parameters based on resolution
#'
#' This function is called by [preprocess_species()] to predicts how many
#' parameters the model is likely to have at a different resolution
#' given a set of stats on the number of cells and their area for each
#' timestep at the current resolution, calculated by `calc_abundance_stats()`
#'
#' `predict_params()` calculates a preliminary estimate based on the inaccurate
#' assumption that the area covered by cells will be the same at the two
#' resolutions. However, when changing the resolution not all of the fine cells
#' underlying occupied coarse cells are occupied thus when increasing the
#' resolution the number of cells are underestimated and when decreasing the
#' resolution they will be overestimated. The amount of bias depends on the
#' configuration of occupied cells with more fragmented occupied areas resulting
#' in greater bias.
#'
#' `adjustment` allows compensating for this bias. `adjustment` is multiplied by
#' both the proportional change in resolution: (res2 - res1)/res1 and by the
#' initial area based estimate and the (possibly negative) result is added
#' to the area based estimate.
#'
#' For example if you are doubling the resolution the proportional change is 1
#' and with an  adjustment of 0.35 you end up adding 35% to the initial
#' estimate.
#'
#' Conversely if you are halving the resolution the proportional change is -.5
#' and you end up subtracting 17.5% from the estimate.
#'
#'
#' @param a_stats output from `calc_abundance_stats()` the only used component
#'   is `area` which is a vector of area in square meters in the unmasked cells
#'   for each timestep.
#' @param res the cell (presumed square) resolution in km.
#' @param adjustment  This is used
#' @return The estimated number of parameters given a resolution of `res`
#' @keywords internal
predict_params <- function(a_stats, res, adjustment = 0.4){
  if(adjustment < 0 || adjustment > 0.5)
    stop("adjustment should be between 0 and 0.5")


  # Predict assuming the same area covered by cells at both resolutions
  res_m <- res * 1000  #
  cell_area <- res_m^2 # sq m
  counts <- a_stats$area / cell_area
  pred <- sum(counts[-length(counts)] * counts[-1], counts[1])

  # Adjust based on the proportional change
  # add the adjustment x proportional change x the prediction
  # proportional change is positive when increasing and negative when
  # decreasing
  change <- (res - a_stats$res) / a_stats$res  # proportional change in resolution
  # change has a theoretical range from -1 to inf.
  adj_pred <- pred  + pred * change * adjustment
  return(adj_pred)
}




