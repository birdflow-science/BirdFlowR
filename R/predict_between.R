#' Predict bird distributions conditioned on known locations
#'
#' `predict_between()` computes marginal probability distributions at each
#' timestep conditioned on observed locations at specific times, using a
#' Hidden Markov Model forward-backward algorithm.
#'
#' @inheritParams route_between
#' @inheritDotParams lookup_timestep_sequence -x
#' @return A matrix of dimensions `n_active(bf)` x `n_query_timesteps` of
#'   marginal probability distributions p(x_t | observations), one column per
#'   timestep. Same format as [predict()]. The log-likelihood of the
#'   observations is stored as attribute `log_z`.
#' @seealso [predict()], [route_between()]
#' @export
#' @importFrom Matrix Matrix t
#' @importMethodsFrom Matrix t %*%
#' @importClassesFrom Matrix Matrix sparseMatrix
predict_between <- function(bf,
                            x_coord = NULL, y_coord = NULL, date = NULL,
                            potentials = NULL,
                            ...) {

  ### BACK COMPATIBILITY CODE
  bf <- add_dynamic_mask(bf)

  dyn_mask <- bf$geom$dynamic_mask

  # --- Input validation and potential list construction ---
  # (identical to route_between)
  using_hard_obs <- !is.null(x_coord) || !is.null(y_coord)
  using_soft_obs <- !is.null(potentials)

  if (using_hard_obs && using_soft_obs)
    stop("Provide either x_coord/y_coord or potentials, not both.")
  if (!using_hard_obs && !using_soft_obs)
    stop("Must provide either x_coord/y_coord (with date) or potentials.")

  if (using_hard_obs) {
    if (is.null(x_coord) || is.null(y_coord))
      stop("x_coord and y_coord must both be provided.")
    if (is.null(date))
      stop("date is required when using x_coord and y_coord.")
    if (length(x_coord) != length(y_coord) ||
        length(x_coord) != length(date))
      stop("x_coord, y_coord, and date must all have the same length.")

    obs_timesteps <- lookup_timestep(date, bf)
    obs_i <- xy_to_i(x_coord, y_coord, bf)
    if (any(is.na(obs_i)))
      stop("One or more coordinates fall outside the model extent or active ",
           "cells.")

    n_act <- n_active(bf)
    potential_list <- lapply(seq_along(obs_timesteps), function(k) {
      phi <- rep(0, n_act)
      phi[obs_i[k]] <- 1
      phi
    })
    names(potential_list) <- as.character(obs_timesteps)

  } else {
    if (nrow(potentials) != n_active(bf))
      stop("potentials must have n_active(bf) = ", n_active(bf), " rows, ",
           "but has ", nrow(potentials), ".")

    if (!is.null(date)) {
      obs_timesteps <- lookup_timestep(date, bf)
      if (length(obs_timesteps) != ncol(potentials))
        stop("Length of date must equal ncol(potentials).")
    } else if (!is.null(colnames(potentials))) {
      obs_timesteps <- lookup_timestep(colnames(potentials), bf)
    } else {
      stop("Timesteps must be specified via column names on potentials or ",
           "the date argument.")
    }

    potential_list <- lapply(seq_len(ncol(potentials)),
                             function(k) potentials[, k])
    names(potential_list) <- as.character(obs_timesteps)
  }

  # --- Computation and query timesteps ---
  obs_range <- range(obs_timesteps)
  comp_timesteps <- lookup_timestep_sequence(bf, start = obs_range[1],
                                             end = obs_range[2])

  dots <- list(...)
  if (length(dots) == 0) {
    query_timesteps <- comp_timesteps
  } else {
    query_timesteps <- lookup_timestep_sequence(bf, ...)
    if (any(!query_timesteps %in% comp_timesteps))
      stop("Query timesteps (from ...) must fall within the range of the ",
           "observations (timesteps ", obs_range[1], " to ",
           obs_range[2], ").")
  }

  # --- Forward-backward ---
  ff <- forward_filter(bf, comp_timesteps, potential_list)
  alphas <- ff$alphas

  betas <- backward_filter(bf, comp_timesteps, potential_list)

  # --- Combine α and β to get marginals ---
  pred <- matrix(0, nrow = n_active(bf), ncol = length(query_timesteps))
  dimnames(pred) <- list(i = NULL, timestep = paste0("t", query_timesteps))

  query_idx <- match(query_timesteps, comp_timesteps)
  for (k in seq_along(query_timesteps)) {
    step <- query_idx[k]
    t_k <- comp_timesteps[step]
    dm_k <- dyn_mask[, t_k]

    gamma <- alphas[[step]] * betas[[step]]
    s <- sum(gamma)
    if (s > 0)
      pred[dm_k, k] <- gamma / s
  }

  attr(pred, "log_z") <- ff$log_z
  return(reformat_distr_labels(pred, bf))
}
