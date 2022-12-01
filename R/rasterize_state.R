#' Function to convert a state vector into a SpatRaster object
#'
#' This function creates a [SpatRaster][terra::SpatRaster] similar to those
#' created by [terra::rast()] from a model state.
#'
#' It may eventually be replaced with one or more rast() methods.
#'
#' @param x A vector of a model state, typically representing the
#'  distribution of a species.
#' @param obj A BirdFlow model.
#' @return A [terra::SpatRaster] object.
#' @export
rasterize_state <- function(x, obj){
  # Note: currently only works with 1 dimensional state
  # should add capacity for matrix input to multi-layered output
  if(length(x) != obj$n)
    stop("Expected state vector to be of length ", obj$n, ", found ", length(x))
  m <- expand_state(x, obj)
  return( terra::rast(m, extent = obj$geom$ext, crs = obj$geom$crs) )
}
