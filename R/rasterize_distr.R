#' @name rasterize
#' @title Convert a BirdFlow distribution into a raster
#' @description `rast()` converts a BirdFlow object directly
#' to a [SpatRaster][terra::SpatRaster].
#' `rasterize_distr()` converts a [distribution](as_distr) into a
#' [SpatRaster][terra::SpatRaster], numeric matrix or array, or a raster data
#'  frame.
#'
#' @param distr A distribution in its vector form or a
#' matrix in which each column represents a different distribution.
#' @param bf A BirdFlow object.
#' @param x A BirdFlow object.
#' @param format One of `'SpatRast'` for a [terra::SpatRaster] object,
#'   `'numeric'` for a matrix or array, or`'dataframe'` for raster data
#'   suitable for plotting with [ggplot2::geom_raster()]
#' @inheritParams get_distr
#' @return
#' For `rasterize_distr()` the return type depends on the `format` argument:
#' * `"SpatRaster"` (the default) returns a [terra::SpatRaster] object.
#' * `"numeric"` returns a matrix (one distribution) or array (multiple
#' distributions).  In either case the first two dimensions will be y (rows),
#' and x (columns).
#' * `"dataframe"` will return the raster information in a data frame with a
#' row for every value (long format) with columns:
#'   * `x`, `y` the x and y coordinates of the cell center.
#'   * `i` the location index (in `bf`) of the cell.
#'   * `label` The label associated with the distribution, taken from column
#'   names of `distr` - typically it indicates time. It is an ordered factor
#'    with the level order matching the order of the distribution in `distr`.
#'    The ordered factor is helpful when animating.
#'   * `value`  The cell value, typically density
#'   * `order`  The column index of the distribution in `distr` or if only one
#'   distribution `1`.
#'   The object is suitable for plotting with [[ggplot2::geom_raster]].
#'
#'
#' @importMethodsFrom terra rast
#' @export
rasterize_distr <- function(distr, bf, format = "SpatRaster") {

  format <- tolower(format)

  # allow alternate output format names
  format <- switch(format,
                   "spatrast" = "spatraster",
                   "terra" = "spatraster",
                   "matrix" = "numeric",
                   "array" = "numeric",
                   "raster.data.frame" = "dataframe",
                   "data.frame" = "dataframe",
                   format
                   )

  stopifnot("Format must be one of 'SpatRaster', 'numeric', or 'dataframe'" =
              format %in% c("spatraster", "numeric", "dataframe"))

  if (format == "dataframe") {
    # Data frame for use with ggplot2 geom_raster() it will
    # have one row per cell in the full raster

    # Get x, y, and i for all cells in the full raster
    #  i will be NA for cells that aren't active
    rows <- seq_len(nrow(bf))
    cols <- seq_len(ncol(bf))
    all <- expand.grid(rows, cols)
    names(all) <- c("row", "col")
    all$x <- col_to_x(all$col, bf)
    all$y <- row_to_y(all$row, bf)
    all$i <- rc_to_i(all$row, all$col, bf)
    mv <- match(1:n_active(bf), all$i)
    n_rast <- ncol(bf) * nrow(bf)

    distr_is_vector <- !is.matrix(distr) && !is.array(distr)

    # Expand distribution out to one row per cell in raster
    if (distr_is_vector) {
      distr2 <- rep(NA, n_rast)
      distr2[mv] <- distr
      distr2 <- data.frame(value = distr2)
      # Add time column if appropriate
      time <- attr(distr, "time")
      if (is.null(time))
        time <- ""
      distr2 <- cbind(data.frame(label = time), distr2)
      df <- cbind(all[, c(-1, -2)], distr2)
      df$label <- ordered(df$label)

    } else {  # multiple distributions
      distr2 <- matrix(NA, nrow = n_rast, ncol = ncol(distr))
      distr2[mv, ] <- distr
      colnames(distr2) <- colnames(distr)
      distr2 <- as.data.frame(distr2)
      df <- cbind(all[, c(-1, -2)], distr2)
      df <-   tidyr::pivot_longer(df,  cols = setdiff(names(df), names(all)),
                                  names_to = "label",
                                  values_to = "value")
      # Label column has original distribution column names as a an ordered
      # factor with the level in the
      df$label <- ordered(df$label, levels = colnames(distr))

    }

    # 1:n_distr  in original column order
    df$order <- as.numeric(df$label)

    return(df)
  }

  m <- expand_distr(distr, bf)

  if (format == "numeric") {
    return(m)
  }

  if (format == "spatraster") {
    r <- terra::rast(m, extent = bf$geom$ext, crs = bf$geom$crs)
    n_dim <- length(dim(m)) # no of dimensions of output
    if (n_dim == 3) # two or more distributions
      names(r) <- dimnames(m)[[3]]
    if (n_dim == 2) {# one distribution
      time <- attr(distr, "time")
      if (!is.null(time) && length(time) == 1)
        names(r) <- time
    }
    return(r)
  }

  stop("Unrecognized format.") # should not ever get here

}

# Note the internal object name below echos S3 class names
# It is a method for rast that works with BirdFlow objects
# nolint start: object_name_linter.
rast.BirdFlow <- function(x, which = "all") {
  rasterize_distr(get_distr(x, which), x)
}
# nolint end
#' @rdname rasterize
#' @return `rast()` returns a [terra::SpatRaster]
#' @export
setMethod(rast, "BirdFlow", rast.BirdFlow)
