#' Truncate the timespan of a BirdFlow model
#'
#' `truncate_birdflow()` Eliminates marginals and/or transitions from a BirdFlow
#' model and adjusts other aspects of the model so that it only covers part of a
#' year.  The intent is to reduce object size and processing time when only part
#' of the year is of interest.
#'
#' The model timesteps will always be numbered from 1 to `n_timesteps()` and so
#' likely will not be consistent with weeks of the year with the truncated
#' model.
#'
#' Currently it's possible to truncate both a fitted model and a model produced
#' by [preprocess_species()] but I have yet to create a way to export a
#'  preprocessed model after truncating it (independently of
#'  [preprocess_species()]) or to truncate during preprocessing. So currently
#'  the utility is limited to reducing the size of fitted models.
#'
#' It's possible to truncate a model over the year boundary but routes generated
#' from such a model will not plot correctly so it's not recommended;
#' see [issue #120](https://github.com/birdflow-science/BirdFlowR/issues/120).
#'
#' @param bf A BirdFlow object.
#' @inheritDotParams lookup_timestep_sequence -x
#' @return A BirdFlow model that only contains information about transitions
#' for a subset of the year as specified by `...`.
#' @export
truncate_birdflow <- function(bf, ...) {

  # Add timestep_padding metadata if it doesn't exist
  ### back compatibility
  if (is.null(bf$metadata$timestep_padding)) {
    ts_padding <- get_timestep_padding(bf)
    bf$metadata$timestep_padding <- ts_padding
  }
  # Add dates$weeks if it doesn't exist
  ### back compatibility code
  if(is.null(bf$dates$week) && nrow(bf$dates) == 52){
    bf$dates$week <- 1:52
  }

  # Handle special case of circular preprocessed model
  # The last date and distribution in these models is a duplicate of the
  # first with an incremented timestep.
  # This is an abnormal state to trick the python code into fitting a transition
  # from the last to the first timestep, but it messes things up when
  # truncating.
  if (bf$dates$date[1] == bf$dates$date[nrow(bf$dates)]) {
    bf$dates <- bf$dates[-nrow(bf$dates), ]
    bf$distr <- bf$distr[, -ncol(bf$distr)]
    bf$geom$dynamic_mask <- bf$geom$dynamic_mask[, -ncol(bf$geom$dynamic_mask)]
  }

  # Define the retained timesteps both by their old index (in the full model)
  # and their new index (in the truncated model)
  old_timesteps <- lookup_timestep_sequence(bf, ...)
  if (any(duplicated(old_timesteps)))
    stop("You must truncate to less than a full year.")
  new_timesteps <- seq_along(old_timesteps)

  # Define the retained transtitions both by their old and new names
  # (forward only)
  old_transitions <- as_transitions(old_timesteps, bf)
  new_transitions <- as_transitions(new_timesteps, bf)

  # Make timestep crosswalk
  ts_cw <- data.frame(old = old_timesteps, new = new_timesteps)

  # Update dates
  d <- bf$dates

  ### back compatibility code
  if (get_metadata(bf, "ebird_version_year") < 2022){
    mv <- match(ts_cw$old, d$interval) # (mv = match vector)
    d <- d[mv, , drop = FALSE] # subset and possibly reorder (for looping models)
    d$interval <- seq_len(nrow(d))
  } else {
    mv <- match(ts_cw$old, d$timestep) # (mv = match vector)
    d <- d[mv, , drop = FALSE] # subset and possibly reorder (for looping models)
    d$timestep <- seq_len(nrow(d))
  }

  rownames(d) <- NULL
  bf$dates <- d

  # Metadata
  bf$metadata$n_timesteps <- nrow(bf$dates)
  bf$metadata$n_transitions <- nrow(bf$dates) - 1

  # Marginals
  if (has_marginals(bf)) {
    mi <- bf$marginals$index
    stopifnot(all(old_transitions %in% mi$transition))

    # Make marginal cross walk (m_cw)
    mv <- match(old_transitions, mi$transition) # match vector
    new_marginals <- gsub("^T", "M", new_transitions)
    m_cw <- data.frame(old = mi$marginal[mv], new = new_marginals)

    marginals <- bf$marginals
    stopifnot(all(m_cw$old %in% names(marginals)))
    m_mv <- match(m_cw$old, names(marginals))
    new_marginals <- marginals[m_mv]  # reorder and subset marginals
    stopifnot(names(new_marginals) == m_cw$old)
    names(new_marginals) <- m_cw$new  # rename

    new_marginals$index <- make_marginal_index(bf) # build new marginal index

    bf$marginals <- new_marginals
  }


  if (has_transitions(bf)) {
    # If there are transitions we need to rename them based on the new
    # timesteps.

    # Make transition cross walk
    # include all possible forward and backwards transitions
    t_cw <- rbind(
      data.frame(old = old_transitions, new = new_transitions),
      data.frame(old = as_transitions(base::rev(old_timesteps), bf),
                new = as_transitions(base::rev(new_timesteps), bf)))

    t <- bf$transitions
    stopifnot(all(names(t) %in% t_cw$old))
    mv <- match(names(t), t_cw$old)
    names(t) <- t_cw$new[mv]
    bf$transitions <- t
  }

  # Subset and rename columns of distr
  d_cw <- data.frame(old = paste0("t", old_timesteps),
                     new = paste0("t", new_timesteps)) # distribution cross walk
  d <- bf$distr
  stopifnot(all(d_cw$old %in% colnames(d)))
  mv <- match(d_cw$old, colnames(d))
  d <- d[, mv, drop = FALSE]
  stopifnot(all(colnames(d) == d_cw$old))
  colnames(d) <- d_cw$new
  bf$distr <- d

  # Subset and rename columns of dynamic mask
  # Reuse d_cw as it's the same for both objects
  if (has_dynamic_mask(bf)) {
    dm <- bf$geom$dynamic_mask
    stopifnot(all(d_cw$old %in% colnames(dm)))
    mv <- match(d_cw$old, colnames(dm))
    dm <- dm[, mv, drop = FALSE]
    stopifnot(all(colnames(dm) == d_cw$old))
    colnames(dm) <- d_cw$new
    bf$geom$dynamic_mask <- dm
  }

  return(bf)
}
