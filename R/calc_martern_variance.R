# Define kernel functions
# Original version used "l" instead of "kl" but that led to a
# warning about partial matching of "l" to "len" when invoked via ...
# in calc_dist_weights(). kl makes it unambiguous.
k_m1 <- function(d, gamma, kl) {
  gamma * (1 + sqrt(3) * d / kl)
}

k_m3 <- function(d, gamma = 40, kl = 40) {
  gamma * (1 + sqrt(3) * d / kl) * exp(-sqrt(3) * d / kl)
}

k_m5 <- function(d, gamma = 40, kl = 40) {
  gamma * (1 + sqrt(5) * d / kl +  5 * d^2 / (3 * l^2)) * exp(-sqrt(5) * d / kl)
}

k_sq <- function(d, gamma, kl) {
  gamma * exp(-0.5 * (d / kl)^2)
}

# Note the argument k is a kernel function to use, presumably one of the above.
# ...  is used to flexibly pass additional parameters (gamma and l through to k)
calc_martern_variance <- function(t, len, k, ...) {
  variance <- k(0, ...) -
    (k(t, ...)^2 + k(len - t, ...)^2 - 2 *
       k(t, ...) * k(len - t, ...) *  k(len, ...) / k(0, ...)) /
    (k(0, ...) * (1 - k(len, ...)^2 / k(0, ...)^2))
  return(pmax(variance, 0))
}
