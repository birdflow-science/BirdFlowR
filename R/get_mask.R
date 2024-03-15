
#' Extract mask from BirdFlow model
#'
#' `get_mask()` extracts the static mask from a BirdFlow model.  The
#' static mask is a logical raster indicating which cells are included in the
#' model (at any timestep).  These are also the cells (in row major order)
#' that correspond with distribution values, and location indices.
#'
#' @param bf A BirdFlow model
#' @param format One of `'SpatRaster'` for a [terra::SpatRaster] object,
#'   `'numeric'` for a matrix or array, or`'dataframe'` for raster data
#'   suitable for plotting with [ggplot2::geom_raster()]
#' @return
#' The return type of `get_mask()`depends on the `format` argument:
#' * `"SpatRaster"` (the default) returns a [terra::SpatRaster] object.
#' * `"numeric"` returns the mask as a matrix.
#' * `"dataframe"` will return a data frame suitable for plotting with
#'   [ggplot2::geom_raster] with columns:
#'   * `row`, `col` the row and column indices of each cell.
#'   * `x`, `y` the x and y coordinates of the cell center.
#'   * `i` the location index (in `bf`) of the cell.
#'   * `mask`   `TRUE` for cells included in the *model*, `FALSE` for excluded
#'   cells.
#' @export
#' @examples
#' bf <- BirdFlowModels::amewoo
#' m <- get_mask(bf)
#'
#'\dontrun{
#' library(terra)
#' plot(m)
#'}
#'
get_mask <- function(bf, format = "SpatRaster") {

  format <- tolower(format)
  format <- switch(format,
                   "spatrast" = "spatraster",
                   "terra" = "spatraster",
                   "matrix" = "numeric",
                   "array" = "numeric",
                   "raster.data.frame" = "dataframe",
                   "data.frame" = "dataframe",
                   format)

  stopifnot("Format must be one of 'SpatRaster', 'numeric', or 'dataframe'" =
              format %in% c("spatraster", "numeric", "dataframe"))

  if (format == "spatraster") {
    m <- bf$geom$mask
    r <- terra::rast(m, extent = bf$geom$ext, crs = bf$geom$crs)
    names(r) <- "mask"
    return(r)
  }

  if (format == "numeric") {
    return(bf$geom$mask)
  }

  if (format == "dataframe") {
    # Data frame for use with ggplot2 geom_raster() it will
    # have one row per cell in the full raster

    # Get x, y, i, and mask columns for all cells in the full raster
    #  i will be NA for cells that aren't in the mask
    rows <- seq_len(nrow(bf))
    cols <- seq_len(ncol(bf))
    df <- expand.grid(rows, cols)
    names(df) <- c("row", "col")
    df$x <- col_to_x(df$col, bf)
    df$y <- row_to_y(df$row, bf)
    df$i <- rc_to_i(df$row, df$col, bf)
    mv <- match(1:n_active(bf), df$i)
    df$mask <- FALSE
    df$mask[mv] <- TRUE
    return(df)
  }

  stop("Unrecognized format.") # should not ever get here
}
