
#' @name has
#' @title Does a BirdFlow object have  marginals, distributions or transitions
#'
#' These are private functions to query the contents of a BirdFlow object and
#' allow for more concise and clear code.
#'
#' @param x a BirdFlow model
#'
#' @return logical indicating the BirdFlow model has the relevant element
#' @examples
has_marginals <- function(x){
  x$metadata$has_marginals
}

#' @rdname has
has_transitions<- function(x){
  x$metadata$has_transitions
}

#' @rdname has
has_distr <- function(x){
  x$metadata$has_distr
}




