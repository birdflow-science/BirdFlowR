#' Generate BirdFlow routes between known locations
#'
#' `route_between()` generates synthetic migration routes conditioned on
#' observed locations at specific times, using a Hidden Markov Model
#' forward-filtering-backward-sampling algorithm.
#'
#' @param bf A BirdFlow object.
#' @param n Number of routes to sample.
#' @param x_coord,y_coord Parallel numeric vectors of observed x and y
#'   coordinates (in the model's CRS). Must be provided together with `date`.
#'   Converted internally to indicator (one-hot) potentials. Cannot be used
#'   together with `potentials`.
#' @param date When using `x_coord`/`y_coord`: a parallel vector of dates
#'   corresponding to each observation. When using `potentials`: a vector of
#'   dates corresponding to the columns of `potentials` (alternative to column
#'   names). Accepts any format recognized by [lookup_timestep()]: date strings
#'   (`"2023-03-29"`), `Date` objects, numeric timestep indices, or
#'   `"t1"`-style strings.
#' @param potentials An `n_active(bf)` x `n_obs` matrix of soft observation
#'   potentials. Each column is a non-negative vector representing the
#'   observation likelihood at a given timestep. Timestep association is via
#'   column names or the `date` argument (exactly one required). Cannot be used
#'   together with `x_coord`/`y_coord`.
#' @inheritDotParams lookup_timestep_sequence -x
#' @return A [BirdFlowRoutes] object. Same format as [route()].
#' @seealso [route()], [predict()]
#' @export
#' @importFrom Matrix Matrix
#' @importMethodsFrom Matrix t %*%
#' @importClassesFrom Matrix Matrix sparseMatrix
route_between <- function(bf, n,
                          x_coord = NULL, y_coord = NULL, date = NULL,
                          potentials = NULL,
                          ...) {

  ### BACK COMPATIBILITY CODE
  bf <- add_dynamic_mask(bf)

  # --- Input validation ---
  using_hard_obs <- !is.null(x_coord) || !is.null(y_coord)
  using_soft_obs <- !is.null(potentials)

  if (using_hard_obs && using_soft_obs)
    stop("Provide either x_coord/y_coord or potentials, not both.")
  if (!using_hard_obs && !using_soft_obs)
    stop("Must provide either x_coord/y_coord (with date) or potentials.")

  # --- Build potential list keyed by timestep ---
  if (using_hard_obs) {
    if (is.null(x_coord) || is.null(y_coord))
      stop("x_coord and y_coord must both be provided.")
    if (is.null(date))
      stop("date is required when using x_coord and y_coord.")
    if (length(x_coord) != length(y_coord) || length(x_coord) != length(date))
      stop("x_coord, y_coord, and date must all have the same length.")

    obs_timesteps <- lookup_timestep(date, bf)

    obs_i <- xy_to_i(x_coord, y_coord, bf)
    if (any(is.na(obs_i)))
      stop("One or more coordinates fall outside the model extent or active cells.")

    n_act <- n_active(bf)
    potential_list <- lapply(seq_along(obs_timesteps), function(k) {
      phi <- rep(0, n_act)
      phi[obs_i[k]] <- 1
      phi
    })
    names(potential_list) <- as.character(obs_timesteps)

  } else {
    # Soft observations
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
      stop("Timesteps must be specified via column names on potentials or the ",
           "date argument.")
    }

    potential_list <- lapply(seq_len(ncol(potentials)), function(k) potentials[, k])
    names(potential_list) <- as.character(obs_timesteps)
  }

  # --- Determine computation timesteps (all steps between first and last obs) ---
  obs_range <- range(obs_timesteps)
  comp_timesteps <- lookup_timestep_sequence(bf, start = obs_range[1],
                                             end = obs_range[2])

  # --- Determine query timesteps (what appears in the output) ---
  dots <- list(...)
  if (length(dots) == 0) {
    query_timesteps <- comp_timesteps
  } else {
    query_timesteps <- lookup_timestep_sequence(bf, ...)
    if (any(!query_timesteps %in% comp_timesteps))
      stop("Query timesteps (from ...) must fall within the range of the ",
           "observations (timesteps ", obs_range[1], " to ", obs_range[2], ").")
  }

  # --- Forward filter ---
  ff <- forward_filter(bf, comp_timesteps, potential_list)
  alphas <- ff$alphas

  # --- Backward sample ---
  trajectory <- backward_sample(bf, comp_timesteps, alphas, n)

  # Subset trajectory rows to query timesteps
  query_idx <- match(query_timesteps, comp_timesteps)
  trajectory <- trajectory[query_idx, , drop = FALSE]

  # --- Format output (mirrors route()) ---
  metadata <- bf$metadata[BirdFlowRoutes_metadata_items]
  rts <- format_trajectory(trajectory, bf, query_timesteps)
  latlon <- xy_to_latlon(rts$x, rts$y, bf)
  rts$lon <- latlon$lon
  rts$lat <- latlon$lat
  rts$timestep <- as.integer(rts$timestep)
  rts$route_type <- "synthetic"
  rts$date <- as.Date(rts$date)
  rts <- BirdFlowRoutes(rts, species = bf$species, metadata = metadata,
                        geom = bf$geom, dates = get_dates(bf),
                        source = "Synthesized from a BirdFlow model",
                        sort_id_and_dates = FALSE)
  return(rts)
}


# Internal backward sampler for forward-filtering-backward-sampling.
#
# @param bf A BirdFlow object (must have dynamic_mask).
# @param timesteps Integer vector of timesteps (same as passed to
#   forward_filter).
# @param alphas List of normalized alpha vectors from forward_filter(),
#   one per timestep, each in the dynamically masked subspace.
# @param n Number of routes to sample.
# @return An integer matrix of dimensions length(timesteps) x n, where values
#   are state-space indices (i) of the bird's location at each timestep.
# @keywords internal
backward_sample <- function(bf, timesteps, alphas, n) {
  dyn_mask <- bf$geom$dynamic_mask
  transitions <- as_transitions(timesteps, bf)
  n_steps <- length(timesteps)

  trajectory <- matrix(NA_integer_, nrow = n_steps, ncol = n)

  # Sample at last timestep from the final alpha
  t_last <- timesteps[n_steps]
  active_last <- which(dyn_mask[, t_last])
  dmi_curr <- sample(length(alphas[[n_steps]]), size = n, replace = TRUE,
                     prob = alphas[[n_steps]])
  trajectory[n_steps, ] <- active_last[dmi_curr]

  # Work backwards: for each step, sample x_t given x_{t+1} and alpha_t
  for (step in rev(seq_along(transitions))) {
    t_curr <- timesteps[step]
    t_next <- timesteps[step + 1]

    dm_curr <- dyn_mask[, t_curr]
    dm_next <- dyn_mask[, t_next]
    active_curr <- which(dm_curr)
    active_next <- which(dm_next)

    # Forward transition matrix T: dim [sum(dm_next) x sum(dm_curr)]
    # T[j, k] = p(x_{t+1}=j | x_t=k)
    tm <- get_transition(bf, transitions[step])

    alpha_curr <- alphas[[step]]

    # Convert sampled state-space indices at t_next back to dm-indices
    i_next <- trajectory[step + 1, ]
    dmi_next <- match(i_next, active_next)

    # For each route r, compute weights proportional to:
    #   p(x_{t+1}=dmi_next[r] | x_t) * alpha_t(x_t)
    # = T[dmi_next[r], ] * alpha_curr   (elementwise over x_t)
    #
    # Extract one row per route from T: result is n x sum(dm_curr)
    T_rows <- as.matrix(tm[dmi_next, , drop = FALSE])

    # Multiply each row by alpha_curr (broadcast)
    weights <- sweep(T_rows, 2, alpha_curr, `*`)

    # Normalize rows and sample
    row_sums <- rowSums(weights)
    if (any(row_sums == 0))
      stop("Backward sampler: zero-probability state encountered at timestep ",
           t_curr, ". A sampled location at timestep ", t_next,
           " may be unreachable from any cell at the previous timestep.")

    weights <- weights / row_sums

    dmi_curr <- apply(weights, 1, function(w) sample(length(w), 1, prob = w))
    trajectory[step, ] <- active_curr[dmi_curr]
  }

  trajectory
}
