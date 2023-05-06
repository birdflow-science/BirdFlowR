
#' @name has
#' @title Does a BirdFlow object have certain compoenents
#'
#' @description These functions return information about the contents of a
#' BirdFlow object.
#'
#'
#' @param x a BirdFlow model
#'
#' @return logical indicating the BirdFlow model has the relevant element
#' @export
has_marginals <- function(x){
  x$metadata$has_marginals
}

#' @rdname has
#' @export
has_transitions<- function(x){
  x$metadata$has_transitions
}

#' @rdname has
#' @export
has_distr <- function(x){
  x$metadata$has_distr
}

#' @rdname has
#' @export
has_dynamic_mask <- function(x){
  ! is.null(x$geom$dynamic_mask) && is.matrix(x$geom$dynamic_mask)
}
