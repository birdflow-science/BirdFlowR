
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
  format_raster_data(bf$geom$mask, bf, format, "mask")
}
