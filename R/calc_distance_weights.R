
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
#' @param kernel The kernel used for calculating the standard deviation
#' in the probability distribution:
#' \describe{
#' \item{`"m3"`}{(default) Matern 3/2. Driven by `gamma` and `kl`.}
#' \item{`"m1"`}{Matern 1/2. Driven by `gamma` and `kl`.}
#' \item{`"m5"`}{Matern 5/2. Driven by `gamma` and `kl`.}
#' \item{`"sq"`}{Squared-exponential (Gaussian). Driven by `gamma` and `kl`.}
#' \item{`"bb"`}{Brownian bridge. Driven by `s1`.}
#' }
#' See [visualize_distance_weights()] to explore how these kernels and their
#' hyperparameters shape the spread.
#' @param gamma Spread magnitude hyperparameter (m^2, a variance) for the
#' Matern-family and squared-exponential kernels (`"m1"`, `"m3"`, `"m5"`,
#' `"sq"`). Ignored for `kernel = "bb"`. The default was tuned by eye with
#' `visualize_distance_weights()`.
#' @param kl Lengthscale hyperparameter (m) for the Matern-family and
#' squared-exponential kernels (`"m1"`, `"m3"`, `"m5"`, `"sq"`). Ignored for
#' `kernel = "bb"`. The default was tuned by eye using
#' `visualize_distance_weights()`.
#' @param s1 Spread magnitude hyperparameter (units of sqrt(m), not m) for
#' the `"bb"` kernel. Ignored for all other kernels. The default was tuned by
#' eye against `visualize_distance_weights()`.
#'
#' @return A vector of weights of the same length as the first three arguments.
#' @seealso [visualize_distance_weights()]
#' @keywords internal
calc_dist_weights <- function(dist_to_line, dist_along_line, line_lengths,
                              radius_m, res_m,
                              kernel = c("m3", "bb", "m1", "m5", "sq"),
                              gamma = 3e10, kl = 9e5, s1 = 200) {

  kernel <- match.arg(kernel)

  sd <- calc_dist_weights_sd(dist_along_line, line_lengths, res_m,
                             kernel = kernel, gamma = gamma, kl = kl, s1 = s1)

  # Calculate weight
  weight <- rep(0, length(sd))
  in_range <- (dist_to_line - radius_m) < 1.96 * sd
  low_cum_prob <- stats::pnorm(dist_to_line[in_range] - radius_m,
                               sd = sd[in_range])
  high_cum_prob <- stats::pnorm(dist_to_line[in_range] + radius_m,
                                sd = sd[in_range])
  weight[in_range] <- high_cum_prob - low_cum_prob

  return(weight)
}

#' Standard deviation of the spread kernel used by [calc_dist_weights()]
#'
#' Computes the standard deviation (in m) of the probability distribution
#' representing uncertainty in a bird's location at a given point along a
#' transition line, for one of the kernels supported by
#' [calc_dist_weights()]. Factored out of [calc_dist_weights()] so
#' [visualize_distance_weights()] can plot the same spread it uses without
#' duplicating the kernel-selection logic.
#'
#' @param dist_along_line How far along the line the point of interest is (m)
#' @param line_lengths How long the line is (m)
#' @param res_m The resolution of the associated bird flow model, used to
#' determine the nugget added to the variance to represent the uncertainty in
#' the starting and ending location of the transition.
#' @inheritParams calc_dist_weights
#' @return A vector of standard deviations (m), the same length as
#' `dist_along_line`.
#' @keywords internal
calc_dist_weights_sd <- function(dist_along_line, line_lengths, res_m,
                                 kernel, gamma, kl, s1) {

  len <- line_lengths  # length of great circle (m)
  t <- dist_along_line # Distance from start of great circle to where the
  # point projects onto the great circle (m)

  # s2 is the standard deviation of the nugget
  s2 <- res_m / 4 # This initial setting of 1/4 the cell width means the
  # cell boundary (orthagonally) is at 2 standard deviations from the cell
  # center, the corners will be at 2.8 SD.

  if (kernel == "bb") {
    # Standard deviation
    sd <- sqrt(s1^2 * t * (len - t) / len + s2^2)  # SD (m) for this point.
  } else {
    k_fun <- get(paste0("k_", kernel))
    sd <- sqrt(calc_martern_variance(t, len, k_fun, gamma = gamma, kl = kl) +
                 s2^2)
  }

  sd
}
