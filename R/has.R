
#' @name has
#' @title Does a BirdFlow object have all marginals, distributions or transitions
#'
#' @description These are private functions to query the contents of a BirdFlow object and
#' allow for more concise and clear code.
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




