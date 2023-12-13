
#' @rdname dimensions
#' @export
#' @return `n_parameters()` the number of of parameters that the BirdFlow model
#' contains or will contain. This is the number of cells in the marginal
#' matrices + the sum of the dynamic mask for the first timestep (number of
#' unmasked cells at the first timestep).  If the model isn't dynamically
#' masked this is equivalent to `n_active(x)^2 * n_transitons(x) + n_active(x)`
n_parameters <- function(x) {
  if (has_dynamic_mask(x)) {
    dm <- x$geom$dynamic_mask
    n_each <- apply(dm, 2, sum) # number of cells included for each timestep
    d <- get_dates(x)

    # Forumla below works with either non-cyclical models or cyclical models
    # that have been preprocessed only and thus have the first distribution
    # duplicated as the last
    if (!is_cyclical(x) || d$date[1] == d$date[nrow(d)]) {
      return(sum(n_each[1], n_each * dplyr::lag(n_each), na.rm = TRUE))

    } else { # cyclical, fitted model
      n_each_lag <- c(n_each[-1], n_each[1])     # cyclical lag
      return(sum(n_each[1], n_each * n_each_lag,  na.rm = TRUE))
    }

  } # end fitted cyclical model

  # No dynamic mask. Every marginal has dimensions n_active x n_active.
  return(n_active(x)^2 * n_transitions(x) + n_active(x))
}
