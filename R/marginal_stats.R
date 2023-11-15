
#' marginal statistics
#'
#' Internal functions to calculate the sum of all marginals and the
#' percent of marginal values that are zero.
#'
#' These are used by sparsify and fix_dead_ends.  marginal_stats is slightly
#' more efficient than calling the other two functions independently.
#'
#' @param bf A BirdFlow model.
#'
#' @return `marginal_stats()` returns a list with
#'  \item{sum}{the sum of all the marginals}
#'  \item{pct_zero}{the percent of the values across all marginals that are
#'  zero.}
#' @keywords internal
marginal_stats <- function(bf) {
  marginal_zeros <- marginal_sums <- rep(0, n_transitions(bf))
  mar_names <- unique(bf$marginals$index$marginal)
  for (i in seq_along(mar_names)) {
    marginal_sums[i] <- sum(bf$marginals[[mar_names[i]]])
    marginal_zeros[i] <- sum(bf$marginals[[mar_names[i]]] == 0)
  }
  pct_zero <- sum(marginal_zeros) / (n_active(bf)^2 * n_transitions(bf)) * 100
  return(list(sum = sum(marginal_sums), pct_zero = pct_zero))
}

#' @rdname marginal_stats
#' @return `sum_marginals()` returns the sum of all marginals.
#' @keywords internal
sum_marginals <- function(bf) {
  marginal_sums <- rep(0, n_transitions(bf))
  mar_names <- unique(bf$marginals$index$marginal)
  for (i in seq_along(mar_names)) {
    marginal_sums[i] <- sum(bf$marginals[[mar_names[i]]])
  }
  return(sum(marginal_sums))
}

#' @rdname marginal_stats
#' @return `calc_pct_zero()` returns the percent of marginal values that are
#' zero
#' @keywords internal
calc_pct_zero <- function(bf) {
  marginal_zeros <- rep(0, n_transitions(bf))
  mar_names <- unique(bf$marginals$index$marginal)
  for (i in seq_along(mar_names)) {
    marginal_zeros[i] <- sum(bf$marginals[[mar_names[i]]] == 0)
  }
  return(sum(marginal_zeros) / (n_active(bf)^2 * n_transitions(bf)) * 100)

}
