if (FALSE) {
  # Example and test code, assumes BirdFlow object bf
  d <- get_distr(bf, 1) # compacted form
  e <- expand_distr(d, bf) # expand

  # Convert to SpatRaster
  library(terra)
  r <- rast(x = e, ext = bf$geom$ext)
  plot(r)

  # Convert to stars object
  library(stars)
  r <- st_as_stars(t(m), crs = bf$geom$crs)
  r <- st_set_dimensions(r, names = c("x", "y"))
  r <- st_set_dimensions(r, "x", offset = bf$geom$ext[1],
                         delta = bf$geom$res[1])
  r <- st_set_dimensions(r, "y", offset = bf$geom$ext[4],
                         delta = -bf$geom$res[2])

  f <- flatten_raster(e, bf)
  stopifnot(all(d == f))
}

#' Convert a raster bird distribution into its flattened, vector equivalent
#'
#' This function converts between a raster representation of data in which rows
#' and columns indicate position in space and a vector representation that
#' contains only the active (not masked) cells in row major order - starting at
#' the top left and proceeding left to right along each row.  The collapsed form
#' is used for projecting the flow model while the expanded form is used to
#' export, import, and visualize the data.
#'
#' @param x Either a matrix representing a single bird distribution or an
#'   array representing multiple distributions with dimensions: row, col, and
#'   distribution.
#' @param bf A `BirdFlow` model.
#'
#' @return Either a vector representing a single distribution in its collapsed
#'   form or, if `x` represents multiple distributions,  a matrix with one
#'   distribution per column.
#' @export
#'
#' @seealso
#'  - [expand_distr()] does the opposite of `flatten_raster()`.
#'  - [rasterize_distr()] has replaced this function in the public API.  With
#'  `format = "numeric"` it returns an identical object to `expand_distr()` but
#'  by default (`format = "SpatRaster" it goes one step further and adds spatial
#'   metdata to make a [terra::SpatRaster].
#'  - [index_conversions] for ways to convert among indexes of the data in
#' raster row and column, index along the flattened vector, and Cartesian space.
#'

flatten_raster <- function(x, bf) {


  # Preserve time attribute if it's present
  # rasterize_distr will add this attribute if it's a single distribution
  # and thus time can't be stored in dimnames.
  dim3_name <- attr(x, "time")

  # Record original format as flags
  is_spatraster <- inherits(x, "SpatRaster")
  is_numeric <- is.numeric(x)

  if (!(is_spatraster || is_numeric)) {
    stop("x must be a SpatRaster or a numeric matrix, or array.")
  }

  if (is_spatraster) {
    compareGeom(x, bf) # will throw error if x and bf don't have matching
                       # extent, res, cellsize, and crs
    n_layers <- terra::nlyr(x)
    r_dimnames <- c(row = NULL, col = NULL, time = names(x))

    # The name of a single layer
    if (n_layers == 1) {
      dim3_name <- names(x)
    }
    x <- terra::as.array(x)

    if (n_layers == 1) {
      # If one layer drop third dimension
      x <- x[,  , 1]
    }

  }

  if (is_numeric) {
    x <- as.array(x)
  }

  # Regardless of how we started
  r_dimnames <- dimnames(x)
  r_dim <- dim(x)
  n_dim <- length(r_dim)
  perm <- 1:n_dim
  perm[1:2] <- 2:1
  x <- aperm(x, perm = perm) # permute the array so first two dimensions are
  # col, row

  a <- x[t(bf$geom$mask)] # transposed so col, row to match permuted x
  if (n_dim == 2) { # 2 d input, 1d output - result is vector
    if (!is.null(dim3_name))
      attr(a, "time") <- dim3_name # assuming "time" is appropriate here
    return(a)
  }

  # 2d or higher output - format vector back into an array
  new_dim <- c(sum(bf$geom$mask), r_dim[3:n_dim])
  a <- array(data = a, dim = new_dim)
  dimnames(a) <- c(list(i = NULL), r_dimnames[-1:-2])

  return(a)

}
