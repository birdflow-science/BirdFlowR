
#' @rdname dimensions
#' @export
#' @return `n_parameters()` the number of of parameters that the BirdFlow model
#' contains or will contain. This is the number of cells in the marginal
#' matrices + the sum of the dynamic mask for the first timestep (number of
#' un masked cells at the first timestep).  If the model isn't dynamically
#' masked this is equivalent to `n_active(x)^2 * n_transitons(x) + n_active(x)`
n_parameters <- function(x) {
  if (has_dynamic_mask(x)) {
    dm <- x$geom$dynamic_mask
    n_each <- apply(dm, 2, sum) # number of cells included for each timestep
    return(sum(n_each[1], n_each * dplyr::lag(n_each), na.rm = TRUE))
  }

  # No dynamic mask. Every marginal has dimensions n_active x n_active.
  return(n_active(x)^2 * n_transitions(x) + n_active(x))
}
