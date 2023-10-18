

#' Convert to a BirdFlow distribution
#'
#' Methods to convert raster or point data to distributions.  Each layer of a
#' raster will be treated as a distribution.  With point data each point will
#' be treated as a "one hot" distribution containing mostly 0 and a single
#' 1 at the location corresponding to the point.
#'
#' If `x` is a data.frame it should have columns `x` and `y` containing the
#' coordinates of points.   `crs` should indicate the coordinate reference
#' system the points are in - only necessary if it differs from `crs(bf)`.
#'
#' If `x` is either an *sf* object or a data frame it should represent points.
#' Each point will be converted into a separate distribution
#' with the cell corresponding to the point having a value of 1 with the
#' remaining values set to 0.
#'
#' If `x` is a [terra::SpatRaster()] it will be projected to align with the
#' cells in `bf` with [terra::project()] using `method = "average"` and then
#' cropped and/or extended to match the extent of `bf`.  Warnings will be throw
#' if some of the vvalue in `x` is lost due to cropping or do to masking out the
#' inactive cells in `bf`.
#'

#' @param x An object to be converted either a data.frame with x and y columns
#' indicating point locations, a *sf* object containing points, or a raster
#' object containing values to be treated as a distribution.
#' @param bf a reference BirdFlow object
#' @param ... arguments used by other methods
#'
#' @return An object containing distribution data to be projected with x.
#' Either a vector with [n_active(bf)](n_active()) values or a matrix with
#' that many rows and a column for each distribution.
#' @export
#'
as_distr <- function(x, bf, ...) {
  UseMethod("as_distr", x)
}

#' @rdname as_distr
#' @param crs The coordinate reference system that the x and y coordinates are
#' in.  If NULL the function will assume coordinates are in the same CRS as
#' `bf`.
#' @export
as_distr.data.frame <- function(x, bf, crs = NULL, ...) {
  if (!all(c("x", "y") %in% names(x)))
    stop("the data.frame x should have columns 'x' and 'y'")

  # transform x and y if crs is set
  if (!is.null(crs)) {
    x_sf <- sf::st_as_sf(x, coords = c("x", "y"), crs = sf::st_crs(crs))
    x_sf <-   sf::st_transform(x_sf, crs = crs(bf))
    coords <- sf::st_coordinates(x_sf)
    x$x <- coords[, 1]
    x$y <- coords[, 2]
  }

  i <- xy_to_i(x$x, x$y, bf)  # cell index

  if (any(is.na(i)))
    warning(paste0(
      "Not all locations in x are within the BirdFlow mask. ",
      "Problem rows: ", paste(which(is.na(i)), collapse = ", "),
      " Affected distributions will have NA for all locations."
    ))


  distr <-  matrix(0, ncol = length(i), nrow = n_active(bf))
  distr[cbind(i, seq_along(i))] <- 1
  if (any(is.na(i)))
    distr[, is.na(i)] <- NA

  if (length(i) == 1)
    distr <- as.vector(distr)

  return(distr)
}

#' @rdname as_distr
#' @param normalize if `TRUE` normalize each distribuiton to sum to 1
#' @param zero_na  if `TRUE` replace `NA` values with `0`.
#' @export
as_distr.SpatRaster <- function(x, bf, normalize = TRUE, zero_na = TRUE, ...) {

  cropped_sum <- NULL
  message <- ""
  raster_names <- names(x)

  if (!terra::compareGeom(x, bf, stopOnError = FALSE)) {
    x <- terra::project(x, rast(bf, 1), method = "average", align = TRUE)

    reprojected_sum <- sum(terra::values(x), na.rm = TRUE)
    x <- terra::crop(x, rast(bf, 1))
    x <- terra::extend(x, rast(bf, 1))

    cropped_sum <- sum(terra::values(x), na.rm = TRUE)
    extent_loss  <- reprojected_sum  - cropped_sum
    if (extent_loss != 0)
      message <- paste0(message, round(extent_loss / reprojected_sum * 100, 2),
                        " of the initial value in the raster was lost while ",
                        "cropping to the bf extent.")
  }

  if (is.null(cropped_sum)) {
    cropped_sum <- reprojected_sum <- sum(terra::values(x), na.rm = TRUE)
  }

  names(x) <- raster_names

  x <- flatten_raster(x, bf)
  inactive_loss <- cropped_sum - sum(x, na.rm = TRUE)

  if (inactive_loss != 0) {
    new_message <- paste0(round(inactive_loss / reprojected_sum * 100, 2),
                          " of the initial value in the raster was lost ",
                          " while masking inactive cells.")

    message <- paste(message, new_message, sep = "\n")
  }

  if (zero_na) {
    x[is.na(x)] <- 0
  }

  if (normalize) {
    if (is.matrix(x)) {
      x <- apply(x, 2, function(x) x / sum(x, na.rm = TRUE))
    } else {
      x <- x / sum(x, na.rm = TRUE)
    }
  }

  if (message != "")
    warning(message)

  return(x)

}

#' @rdname as_distr
#' @export
as_distr.sf <- function(x, bf, ...) {

  if (!all(sf::st_geometry_type(x) == "POINT"))
    stop("as_distr() works with sf objects that have only POINTS/ ",
         "x is sf but contains other geometery types")

  x <- sf::st_transform(x, sf::st_crs(bf))

  x <- sf::st_coordinates(x) |> as.data.frame()
  names(x) <- c("x", "y")

  as_distr(x, bf)

}
