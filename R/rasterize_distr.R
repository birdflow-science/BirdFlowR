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
#' @param format One of `'SpatRast'` for a [terra::SpatRaster] object,
#'   `'numeric'` for a matrix or array, or`'data.frame'` for raster data
#'   suitable for plotting with [ggplot2::geom_raster()]
#' @inheritParams get_distr
#' @return A [terra::SpatRaster] object.
#' @importMethodsFrom terra rast
#' @export
rasterize_distr <- function(distr, bf, format = "SpatRast"){

  format <- tolower(format)

  # allow alternate output format names
  format <- switch(format,
                   "terra" = "spatrast",
                   "matrix" = "numeric",
                   "array" = "numeric",
                   "raster.data.frame" = "data.frame",
                   format
                   )

  stopifnot("Format must be one of 'SpatRast', 'numeric', or 'data.frame'" =
              format %in% c("spatrast", "numeric","data.frame") )

  if(format == "data.frame"){
    # Data frame for use with ggplot2 geom_raster() it will
    # have one row per cell in the full raster

    # Get x, y, and i for all cells in the full raster
    #  i will be NA for cells that aren't active
    rows <- 1:nrow(bf)
    cols <- 1:ncol(bf)
    all <- expand.grid(rows, cols)
    names(all) <- c("row", "col")
    all$x <- col_to_x(all$col, bf)
    all$y <- row_to_y(all$row, bf)
    all$i <- rc_to_i(all$row, all$col, bf)
    mv <- match(1:n_active(bf), all$i)
    n_rast <- ncol(bf) * nrow(bf)

    distr_is_vector <- !is.matrix(distr) && !is.array(distr)

    # Expand distribution out to one row per cell in raster
    if(distr_is_vector){
      distr2 <- rep(NA, n_rast)
      distr2[mv] <- distr
      distr2 <- data.frame(density = distr2)
      # Add time column if appropriate
      time <- attr(distr, "time")
      if(!is.null(time))
        distr2 <- cbind(data.frame(time = time), distr2)
      df <- cbind(all[ , c(-1, -2)], distr2)
    } else {
      distr2 <- matrix(NA, nrow = n_rast, ncol = ncol(distr))
      distr2[mv, ] <- distr
      colnames(distr2) <- colnames(distr)
      distr2 <- as.data.frame(distr2)
      df <- cbind(all[ , c(-1, -2)], distr2)
      df <-   tidyr::pivot_longer(df,  cols = setdiff(names(df), names(all)),
                                  names_to = "time",
                                  values_to = "density")
    }



    return(df)
  }

  m <- expand_distr(distr, bf)

  if(format == "numeric"){
    return(m)
  }

  if(format == "spatrast"){
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

  stop("Unrecognized format.") # should not ever get here

}


rast.BirdFlow <- function(x, which = "all"){
  rasterize_distr(get_distr( x, which), x)
}
#' @rdname rasterize
#' @export
setMethod(rast, "BirdFlow", rast.BirdFlow)
