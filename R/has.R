
#' @name has
#' @title Does a BirdFlow object have certain components
#'
#' @description These functions report whether a BirdFlow object has
#' certain optional components:
#' * `has_marginals()` indicates whether `x` has marginals, the canonical,
#'   smaller representation of the model's dynamics.
#' * `has_transitions()` indicates whether `x` has precomputed transitions,
#'   derived from the marginals, which is what [predict()] and [route()]
#'   actually multiply against distributions.
#' * `has_distr()` indicates whether `x` has stored eBird Status & Trends
#'   distributions, separate from the marginals/transitions, from the data
#'   the model was trained on.
#' * `has_dynamic_mask()` indicates whether `x` has a dynamic
#'   (per-timestep) mask, in addition to the static mask every BirdFlow
#'   object has.
#'
#' @param x A BirdFlow model
#'
#' @return Logical indicating the BirdFlow model has the relevant element
#' @export
has_marginals <- function(x) {
  x$metadata$has_marginals
}

#' @rdname has
#' @export
has_transitions <- function(x) {
  x$metadata$has_transitions
}

#' @rdname has
#' @export
has_distr <- function(x) {
  x$metadata$has_distr
}

#' @rdname has
#' @export
has_dynamic_mask <- function(x) {
  ! is.null(x$geom$dynamic_mask) && is.matrix(x$geom$dynamic_mask)
}
