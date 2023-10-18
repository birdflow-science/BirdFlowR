

#' Convert a BirdFlow object without a dynamic mask into one with a dynamic mask
#'
#' A dynamic mask is a logical matrix of the same dimensions as the distr matrix
#' for `bf` holding TRUE for cells where the corresponding location (row) and
#' time (column) is included in the model.
#'
#' @param bf a BirdFlow object
#' @param dummy_mask if TRUE a mask is addded to the object, but the mask is
#' TRUE for every cell. This yields a BirdFlow object that works with the
#' current mask dependent version of the package but mimics and old BirdFlow
#' model.  Note if the old model included state based sparsification the
#' predictions should be identical even with `dummy_mask = FALSE` (the default).
#'
#' @return a BirdFlow object that has a dynamic_mask component and in which
#' the marginals only includes transitions between cells that are not
#' dynamically masked.
#'
#' @export
#' @examples
#' bf <- add_dynamic_mask(BirdFlowModels::amewoo)
#'
add_dynamic_mask <- function(bf, dummy_mask = FALSE) {
  if (has_dynamic_mask(bf))
    return(bf)

  if (!has_marginals(bf))
    stop("bf must have marginals to add a dynamic mask.")
  if (!has_distr(bf))
    stop("bf must have distributions to add a dynamic mask.")

  had_transitions <- has_transitions(bf)
  if (had_transitions)
    bf <- drop_transitions(bf)

  # make dynamic mask
  dyn_mask <- bf$distr != 0 # Avoid get_distr() here - might change col names
  if (dummy_mask) {
    dyn_mask[, ] <- TRUE
  }
  bf$geom$dynamic_mask <- dyn_mask

  # Index of forward transitions only
  index <- bf$marginals$index
  index <- index[index$direction == "forward", ]
  marginal_names <- index$marginal
  stopifnot(all(marginal_names %in% names(bf$marginals)))
  if (any(duplicated(marginal_names)))
    stop("Shouldn't have duplicated marginals in index")

  # Subset marginals to transitions among unmasked cells.
  for (i in seq_along(marginal_names)) {
    m_name <- marginal_names[i]
    mar <- bf$marginals[[m_name]]

    from  <-  index$from[index$marginal == m_name] # previous distribution index
    to <- index$to[index$marginal == m_name] # next distribution index
    mar <- mar[dyn_mask[, from],  dyn_mask[, to]]
    mar <- mar / sum(mar, na.rm = TRUE) # re-standarize to sum to 1
    bf$marginals[[m_name]] <- Matrix::Matrix(mar, sparse = TRUE)
  } # end marginal loop

  if (had_transitions)
    bf <- build_transitions(bf)

  return(bf)
}
