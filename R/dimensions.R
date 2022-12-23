#' @rdname dimensions
#' @name dimensions
#' @title Dimensions of a BirdFlow object
#' @description
#' Functions to return BirdFlow model dimensions
#'
#' @aliases nrow ncol dim ext
#' @param x A BirdFlow object
#'
#' @return `nrow()` returns the number of rows in the raster extent of the BirdFlow model
#' @method nrow BirdFlow
#' @export
nrow.BirdFlow <- function(x){
  x$geom$nrow
}

#' @rdname dimensions
#' @method ncol BirdFlow
#' @export
#' @return `ncol()` returns the number of columns in the raster extent of a BirdFlow model
ncol.BirdFlow <- function(x){
  x$geom$ncol
}

#' @rdname dimensions
#' @method dim BirdFlow
#' @export
#' @return `dim()` returns the number of rows and columns in the raster extent of a BirdFlow model
dim.BirdFlow <- function(x){
  c(x$geom$nrow, x$geom$ncol)
}

#' @rdname dimensions
#' @export
#' @return `n_timesteps()` returns the number of timesteps in a BirdFlow model
n_timesteps <- function(x){
  x$n_timesteps
}

#' @rdname dimensions
#' @export
#' @return `n_transitions()` returns the number of transitions in a BirdFlow model
n_transitions <- function(x){
  x$n_trans
}

#' @rdname dimensions
#' @export
#' @return `n_active()` returns the number of active cells (locations)
#' in a BirdFlow model.
n_active <- function(x){
 x$n_active
}



