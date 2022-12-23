#' Function to convert a state vector into a SpatRaster object
#'
#' This function creates a [SpatRaster][terra::SpatRaster] similar to those
#' created by [terra::rast()] from one or more distributions in their compact
#' vector form.
#'
#' @param x A distribution in its vector form or a matrix in which each column
#' represents a different distribution.
#' @param obj A BirdFlow model.
#' @return A [terra::SpatRaster] object.
#' @export
rasterize_distr <- function(x, obj){
  m <- expand_distr(x, obj)
  r <- terra::rast(m, extent = obj$geom$ext, crs = obj$geom$crs)
  if(length(dim(m)) == 3)
    names(r) <- dimnames(m)[[3]]
  return(r)
}
