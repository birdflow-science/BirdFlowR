# Internal backward filter for the forward-backward algorithm.
# Used by predict_between() to compute backward messages β_t.
#
# The backward message:
# β_t(x_t) = Σ_{x_{t+1}} p(x_{t+1}|x_t) · φ_{t+1}(x_{t+1}) · β_{t+1}(x_{t+1})
#
# In matrix form: β_t = t(T_{t→t+1}) · (φ_{t+1} ⊙ β_{t+1})
#
# This uses the TRANSPOSE of the forward transition matrix, not the stored
# backward transition matrix. The stored backward matrix is p(x_t | x_{t+1})
# (Bayes-inverted), whereas here we need p(x_{t+1} | x_t) transposed.
#
# Potentials are applied at t+1 when stepping back to t, so each φ_t appears
# exactly once across the α·β product (forward_filter applies φ_t to α_t).
#
# @param bf A BirdFlow object (must already have dynamic_mask added).
# @param timesteps Integer vector of consecutive timesteps (same as passed to
#   forward_filter).
# @param potentials Named list of observation potentials. Names are timesteps
#   as character strings; values are non-negative vectors of length
#   n_active(bf). Same format as in forward_filter().
# @return A list of normalized β vectors, one per timestep, each in the
#   dynamically masked subspace for that timestep.
# @keywords internal
backward_filter <- function(bf, timesteps, potentials) {
  dyn_mask <- bf$geom$dynamic_mask
  transitions <- as_transitions(timesteps, bf)
  n_steps <- length(timesteps)

  betas <- vector("list", n_steps)

  # Initialize at last timestep: β_T = uniform.
  # φ_T is already handled in forward_filter (applied to α_T), so we do not
  # apply it here.
  t_last <- timesteps[n_steps]
  dm_last <- dyn_mask[, t_last]
  beta <- rep(1, sum(dm_last))
  beta <- beta / sum(beta)
  betas[[n_steps]] <- beta

  for (step in rev(seq_along(transitions))) {
    t_next <- timesteps[step + 1]
    dm_next <- dyn_mask[, t_next]

    # Apply φ_{t+1} to β_{t+1} before the backward message pass
    phi_key <- as.character(t_next)
    if (!is.null(potentials[[phi_key]])) {
      beta_weighted <- beta * potentials[[phi_key]][dm_next]
    } else {
      beta_weighted <- beta
    }

    # β_t = t(T_{t→t+1}) · (φ_{t+1} ⊙ β_{t+1})
    tm <- get_transition(bf, transitions[step])
    beta_new <- as.numeric(Matrix::t(tm) %*% beta_weighted)

    s <- sum(beta_new)
    if (s == 0)
      stop("Backward filter collapsed to zero probability at timestep ",
           timesteps[step], ". Observations may be incompatible with the ",
           "model.")
    beta_new <- beta_new / s
    betas[[step]] <- beta_new

    beta <- beta_new
  }

  betas
}
