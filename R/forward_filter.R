# Internal forward filter for HMM inference over BirdFlow models.
# Shared by route_between() (forward-filtering-backward-sampling) and
# predict_between() (forward-backward algorithm, to be implemented).
#
# Uses the scaled forward algorithm: at each step, alpha is normalized to sum
# to 1 and a log scale factor is accumulated. This is equivalent to working in
# log-space with logsumexp but works directly with sparse matrix operations.

# @param bf A BirdFlow object (must already have dynamic_mask added).
# @param timesteps Integer vector of consecutive timesteps to filter over.
# @param potentials Named list of observation potentials. Names are timesteps
#   as character strings; values are non-negative vectors of length
#   n_active(bf). Timesteps not in the list are treated as unobserved
#   (potential = 1 everywhere).
# @return A list with:
#   \item{alphas}{List of normalized alpha vectors (one per timestep), each in
#     the dynamically masked subspace for that timestep.}
#   \item{log_z}{Cumulative log normalization constant (log-likelihood of
#     observations). Useful for predict_between().}
# @keywords internal
forward_filter <- function(bf, timesteps, potentials) {
  dyn_mask <- bf$geom$dynamic_mask
  transitions <- as_transitions(timesteps, bf)

  alphas <- vector("list", length(timesteps))

  t_start <- timesteps[1]
  dm <- dyn_mask[, t_start]
  log_z <- 0

  # Initialize alpha at first timestep.
  # If there is a potential here, use it directly (e.g. a one-hot for a known
  # starting location). Otherwise start uniform.
  phi_key <- as.character(t_start)
  if (!is.null(potentials[[phi_key]])) {
    alpha <- potentials[[phi_key]][dm]
  } else {
    alpha <- rep(1, sum(dm))
  }

  s <- sum(alpha)
  if (s == 0)
    stop("Forward filter: potential at first timestep (", t_start, ") has ",
         "zero mass. Check that the starting location is within the model ",
         "extent.")
  log_z <- log_z + log(s)
  alpha <- alpha / s
  alphas[[1]] <- alpha

  for (step in seq_along(transitions)) {
    tm <- get_transition(bf, transitions[step])
    alpha <- as.numeric(tm %*% alpha)

    t_next <- timesteps[step + 1]
    phi_key <- as.character(t_next)
    if (!is.null(potentials[[phi_key]])) {
      dm_next <- dyn_mask[, t_next]
      alpha <- alpha * potentials[[phi_key]][dm_next]
    }

    s <- sum(alpha)
    if (s == 0)
      stop("Forward filter collapsed to zero probability at timestep ",
           t_next, ". Observations may be incompatible with the model ",
           "(e.g. a location in a masked cell, or unreachable given the ",
           "transition probabilities).")
    log_z <- log_z + log(s)
    alpha <- alpha / s
    alphas[[step + 1]] <- alpha
  }

  list(alphas = alphas, log_z = log_z)
}
