if (FALSE) {
  # Example and test code, assumes BirdFlow object bf
  d <- get_distr(bf, 1) # compacted form
  e <- expand_distr(d, bf) # expand

  # Convert to SpatRaster
  library(terra)
  r <- rast(raster = e, ext = bf$geom$ext)
  plot(r)

  # Convert to stars object
  library(stars)
  r <- st_as_stars(t(m), crs = bf$geom$crs)
  r <- st_set_dimensions(r, names = c("x", "y"))
  r <- st_set_dimensions(r, "x", offset = bf$geom$ext[1], delta = bf$geom$res[1])
  r <- st_set_dimensions(r, "y", offset = bf$geom$ext[4], delta = -bf$geom$res[2])

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
#' @param raster Either a matrix representing a single bird distribution or an
#'   array representing multiple distributions with dimensions: row, col, and
#'   distribution.
#' @param bf A `BirdFlow` model or the `geom` component of one.
#'
#' @return Either a vector representing a single distribution in its collapsed
#'   form or, if `x` represents multiple distributions,  a matrix with one
#'   distribution per column.
#' @export
#'
#' @seealso
#'  - [expand_distr()] does the opposite of `flatten_raster()`.
#'  - [rasterize_distr()] goes one step further and adds spatial metdata to make
#' a [terra::SpatRaster].
#'  - [index_conversions] for ways to convert among indexes of the data in
#' raster row and column, index along the flattened vector, and Cartesian space.
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' d <- get_distr( bf, 1)
#' r <- expand_distr(d, bf) # convert to raster
#' f <- flatten_raster(r, bf) # convert back to distribution
#' stopifnot(all(d == f))
flatten_raster <- function(raster, bf) {
  if ("geom" %in% names(bf)) {
    bf <- bf$geom
  }
  raster <- as.array(raster)
  r_dimnames <- dimnames(raster)
  r_dim <- dim(raster)
  n_dim <- length(r_dim)

  perm <- 1:n_dim
  perm[1:2] <- 2:1
  raster <- aperm(raster, perm = perm) # permute the array so first two dimensions are
  # row, col
  a <- raster[t(bf$mask)]
  if (n_dim == 2) { # 2 d input, 1d output - result is vector
    return(a)
  }

  # 2d or higher output - format vector back into an array
  new_dim <- c(sum(bf$mask), r_dim[3:n_dim])
  a <- array(data = a, dim = new_dim)
  dimnames(a) <- c(list(i = NULL ), r_dimnames[-1:-2])

  return(a)

}
