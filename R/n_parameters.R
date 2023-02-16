
n_parameters <- function(bf){
  # With n_active x n_active marginals:
  n_active(bf)^2 * n_transitions(bf) + n_active(bf)
}
