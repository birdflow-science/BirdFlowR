
# Brownian bridge based weighting. This is a placeholder for now to allow
# implementing weight_betweeness.

#'  calculate the weights of transitions for bmtr points
#'
#'  `calc_dist_weights()` is an internal function that takes summary stats
#'  on the relationship between points and a transition line and returns
#'  the weight that should be used for that transition.
#'
#'  The first three arguments can all be vectors in which case the calculations
#'  will be vectorized over the corresponding elements.
#'
#'  This is a preliminary version of the function and will likely change.
#'
#' @param dist_to_line How far is the point from the line (m)
#' @param dist_along_line How far along the line is the point, after
#' projecting it  onto the line (m)
#' @param line_lengths How long is the line (m)
#' @param radius_m The radius of the transect at the bmtr points - used to
#' determine the band of probability density that will be added to form
#' the weight.
#' @param res_m The resolution of the associated bird flow model, used to
#' determine the nugget added to the variance to represent the uncertainty in
#' the starting and ending location of the transition.
#'
#' @param method  The method used for calculating the standard deviation
#' in the probability distribution.  Currently `"m3"`, Martern 3/2; and
#' `"bb"`, Brownian bridge are supported.
#'
#' @return A vector of weights of the same length as the first three arguments.
#' @keywords internal
calc_dist_weights <- function(dist_to_line, dist_along_line, line_lengths,
                              radius_m, res_m, method = "m3") {



  # Spatial information
  len <- line_lengths / 1000  # length of great circle in km
  t <- dist_along_line / 1000 # Distance from start of great circle to where
  # the point projects onto the great circle in km
  d <- dist_to_line / 1000 # Distance from reference point to the great circle

  res_km <- res_m / 1000

  r <- radius_m / 1000  # Radius of transact in KM.

  # s2 is the standard deviation of the nugget
  s2 <- res_km / 4 # This initial setting of 1/4 the cell width converted
  # to KM means the cell boundary (orthagonally) is at 2
  #  standard deviations from the cell center, the corners
  # will be at 2.8 SD.

  valid_methods <- c("bb", # Brownian Bridge
                     "m3") # Martern 3/2

  if (!method %in% valid_methods)
    stop("Method shold be one of ", paste(valid_methods, collapse = ", "))

  if (method == "bb") {

    # Hyperparameters
    s1 <-  10 # Tune this visually looking at a few resulting distributions?


    # Standard deviation
    sd <-  sqrt(s1^2 * t * (len - t) / len + s2^2)  # SD in KM for this point.

  }

  if (method == "m3") {
    sd <- sqrt(calc_martern_variance(t, len, k_m3, gamma = 40000, kl = 2000) +
                 s2^2)
  }


  # Calculate weight
  weight <- rep(0, length(sd))
  in_range <- (d - r) < 1.96 * sd
  low_cum_prob <- stats::pnorm(d[in_range] - r, sd = sd[in_range])
  high_cum_prob <- stats::pnorm(d[in_range] + r, sd = sd[in_range])
  weight[in_range] <- high_cum_prob - low_cum_prob

  return(weight)
}
