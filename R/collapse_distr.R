if (FALSE) {
  # Example and test code, assumes BirdFlow object bf
  d <- get_distr(1, bf) # compacted form
  e <- expand_distr(d, bf) # expand

  # Convert to SpatRaster
  library(terra)
  r <- rast(x = e, ext = bf$geom$ext)
  plot(r)

  # Convert to stars object
  library(stars)
  r <- st_as_stars(t(m), crs = bf$geom$crs)
  r <- st_set_dimensions(r, names = c("x", "y"))
  r <- st_set_dimensions(r, "x", offset = bf$geom$ext[1], delta = bf$geom$res[1])
  r <- st_set_dimensions(r, "y", offset = bf$geom$ext[4], delta = -bf$geom$res[2])

  f <- collapse_distr(e, bf)
  stopifnot(all(d == f))
}

#' Function to convert a raster bird distribution into its collapsed, vector
#' equivalent
#'
#' This function is primarily for internal use. It converts between the raster
#' representation of data in which rows and columns indicate position in space
#' and a vector representation that contains all the non-masked cells in row
#' major order - starting at the top left and proceeding left to right along
#' each row.  The collapsed form is used for projecting the flow model while the
#' expanded form is used to export, import, and visualize the data.
#'
#' @param x Either a matrix representing a single bird distribution or an array
#' representing multiple distributions.
#' @param bf A `BirdFlow` model or the `geom` component of one.
#'
#' @return Either a vector representing a single distribution in its collapsed
#' form or, if x represents multiple distributions,  a matrix with one
#' distribution per column.
#'
#' @export
#'
#' @seealso
#'  - [expand_distr] does the opposite of `collapse_distr`.
#'  - [index_conversions] converts among indexes of the data in expanded and
#'  collapsed formats, and Cartesian space.
#'
#' @examples
#' \dontrun{
#' # Requires BirdFlow object 'bf'
#' d <- get_distr(1, bf) # compacted form
#' e <- expand_distr(d, bf) # expand
#' f <- collapse_distr(e, bf)
#' stopifnot(all(d == f))
#' }
collapse_distr <- function(x, bf) {
  if ("geom" %in% names(bf)) {
    bf <- bf$geom
  }
  x <- as.array(x)
  xdim <- dim(x)
  ndim <- length(xdim)
  perm <- 1:length(dim(x))
  perm[1:2] <- 2:1
  x <- aperm(x, perm = perm) # permute the array so first two dimensions are
  # row, col
  a <- x[t(bf$mask)]
  if (ndim == 2) { # 2 d input, 1d output - result is vector
    return(a)
  }

  # 2d or higher output - format vector back into an array
  new_dim <- c(sum(bf$mask), xdim[3:length(xdim)])
  a <- array(data = a, dim = new_dim)
}
