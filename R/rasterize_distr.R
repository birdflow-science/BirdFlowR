#' @name rasterize
#' @title Functions to convert a BirdFlow object or distribution into a SpatRaster
#' @description `rasterize_distr()` creates a [SpatRaster][terra::SpatRaster] similar to those
#' created by [terra::rast()] from one or more distributions in their compact
#' vector form. `rast()` converts a BirdFlow object directly to a
#' [SpatRaster][terra::SpatRaster].
#'
#' @param distr a distribution in its vector form or a
#' matrix in which each column represents a different distribution.
#' @param bf A BirdFlow object.
#' @param x A BirdFlow object
#' @inheritParams get_distr
#' @return A [terra::SpatRaster] object.
#' @importMethodsFrom terra rast
#' @export
rasterize_distr <- function(distr, bf){
  m <- expand_distr(distr, bf)
  r <- terra::rast(m, extent = bf$geom$ext, crs = bf$geom$crs)
  n_dim <- length(dim(m)) # no of dimensions of output
  if(n_dim == 3) # two or more distributions
    names(r) <- dimnames(m)[[3]]
  if(n_dim == 2){# one distribution
    time <- attr(distr, "time")
    if(!is.null(time) && length(time) == 1)
      names(r) <- time
  }
  return(r)
}


rast.BirdFlow <- function(x, which = "all"){
  rasterize_distr(get_distr( x, which), x)
}
#' @rdname rasterize
#' @export
setMethod(rast, "BirdFlow", rast.BirdFlow)
