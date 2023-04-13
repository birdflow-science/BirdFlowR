n_parameters <- function(bf) {
  if (has_dynamic_mask(bf)) {
    dm <- bf$geom$dynamic_mask
    n_each <- apply(dm, 2, sum) # number of cells included for each timestep
    return(sum(n_each[1], n_each * dplyr::lag(n_each), na.rm = TRUE))
  }

  # No dynamic mask. Every marginal has dimensions n_active x n_active.
  return(n_active(bf)^2 * n_transitions(bf) + n_active(bf))
}
