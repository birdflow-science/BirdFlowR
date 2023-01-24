#' @name rasterize
#' @title Functions to convert a BirdFlow object or distribution into a SpatRaster
#' @description `rasterize_distr()` creates a [SpatRaster][terra::SpatRaster] similar to those
#' created by [terra::rast()] from one or more distributions in their compact
#' vector form. `rast()` converts a BirdFlow object directly to a
#' [SpatRaster][terra::SpatRaster].
#'
#' @param x For `rasterize_distr()` a distribution in its vector form or a
#' matrix in which each column represents a different distribution.
#' For `rast()` a BirdFlow object.
#' @param bf A BirdFlow object.
#' @inheritParams get_distr
#' @return A [terra::SpatRaster] object.
#' @importMethodsFrom terra rast
#' @export
rasterize_distr <- function(x, bf){
  m <- expand_distr(x, bf)
  r <- terra::rast(m, extent = bf$geom$ext, crs = bf$geom$crs)
  if(length(dim(m)) == 3)
    names(r) <- dimnames(m)[[3]]
  return(r)
}


rast.BirdFlow <- function(x, which = "all"){
  rasterize_distr(get_distr(which, x), x)
}
#' @rdname rasterize
#' @export
setMethod(rast, "BirdFlow", rast.BirdFlow)
