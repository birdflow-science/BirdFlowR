#' function to determine the extent of a SpatRaster
#'
#' This function returns a logical single layer SpatRaster
#' which is TRUE if `x` has non-zero values in any layer.
#' NA does not count as non-zero.  The result is cropped to the extent of the
#' data in `x`. It's used in Birdflow to preprocess data.
#'
#' `get_data_mask` is currently a public function but may be made private in
#' future versions of the package.
#'
#' @param x a SpatRaster, typically it's multilayered and containins the
#' distribution of a species over time.
#' @return a single layer SpatRaster with the same CRS, alignment, and
#' resolution as x; cropped to the extent of the data in `x`; and with
#' a `TRUE` if there are non-zero cells in any layer in `x` and `FALSE`
#' otherwise.
#' @export
#' @keywords internal
make_mask <- function(x, clip){
  # Clips to extent of the non-zero data
  m <- terra::values(x, mat = TRUE)
  m[is.nan(m)] <- 0
  max <- apply(m, 1, max)
  mask <- as.logical(max)  # TRUE if cell has data at any timestep
  mask_mat <- matrix(mask, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  mask_rast <- terra::rast(mask_mat, extent = ext(x), crs = crs(x))

  # Rows and columns corresponding to maximum extent of data
  r  <- which(apply(mask_mat, 1, any))
  c  <- which(apply(mask_mat, 2, any))

  return(mask_rast[r, c, , drop = FALSE])
}
