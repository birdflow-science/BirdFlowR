if (FALSE) {
  full_bf <- import_birdflow(
    "../Models/Visualization/original/batch1/round1/robgro_2021_80km.hdf5")
  sparse_bf <- sparsify(full_bf, "state+conditional", p = .99)
  bf <-  build_transitions(sparse_bf)
}

#' Add or drop transition matrices
#'
#' Given a BirdFlow object with marginals and without transitions
#' `build_transitions()` return a BirdFlow object with both marginals
#' and transitions, `drop_tansitions()` will reverse the process.
#'
#' @rdname build_transitions
#' @param x BirdFlow object
#' @param rebuild Set to TRUE to rebuild transitions if they are already
#' present.
#' @return BirdFlow object with transition matrices
#' @export
#' @seealso [has_transitions()]
#' @examples
#' \dontrun{
#' bf1 <- BirdFlowModels::amewoo
#' bf2 <- build_transitions(bf)
#' bf2
#'
#' bf3 <- drop_transitions(bf2)
#' bf3
#' }
build_transitions <- function(x, rebuild = FALSE) {
  if (has_transitions(x) && !rebuild)
    stop("x already has transitions.")
  if (!has_marginals(x))
    stop("marginals are missing and necessary to build transitions.")
  mi <- x$marginals$index
  tl <- vector(mode = "list", length = nrow(mi))
  names(tl) <- mi$transition
  for (i in seq_len(nrow(mi))) {
    tl[[i]] <- get_transition(x, mi$transition[i])
  }
  x$transitions <- tl
  x$metadata$has_transitions <- TRUE
  return(x)
}

#' @rdname build_transitions
#' @aliases drop_transitions
#' @export
drop_transitions <- function(x) {
  if (!has_marginals(x))
    stop("Cannot drop transitions from a BirdFlow object that lacks marginals.")
  x$transitions <- NA
  x$metadata$has_transitions <- FALSE
  return(x)
}
