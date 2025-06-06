#' Calculate great circle distances among BirdFlow object cells
#'
#' This function calculates a square, distance matrix with `n_active()` cells
#' on each side. Each cell represents the distance between its
#' row index and column index treated as locations in the state space; e.g.
#' The cell  `[3, 8]` represents the distance between locations 3 and 8 in the
#' state space vector.
#'
#' @param bf a BirdFlow object
#'
#' @return a n x n matrix containing pairwise distances in kilometers between
#' every active cell in the BirdFlow object.
#' @keywords internal
#' @export
#' @examples
#' # Calculate great circle distance matrix
#' library(terra)
#' bf <- BirdFlowModels::rewbla
#' gs <- great_circle_distances(bf)
#'
#' # The above is all you need to use the function.
#' # The rest of this is an exercise to visualize the error in CRS distances
#'
#' # Compare to euclidean distances in projected CRS space
#' i <- seq_len(n_active(bf))
#' xy <- as.data.frame(i_to_xy(i, bf))
#' eu <- as.matrix(dist(xy)) / 1000 # euclidean in CRS (km)
#'
#' error_prop <- abs(gs - eu) / gs
#' error_prop[is.nan(error_prop)] <- 0
#'
#' mean_error <- apply(error_prop, 1, mean) # mean error for cell
#' max_error <- apply(error_prop, 1, max) # max error for cell
#'
#' # Calculate maximum error lines for each cell
#' mel <- data.frame(i1 = 1:n_active(bf), i2 = apply(error_prop, 1, which.max))
#' xs <- t(cbind(i_to_x(mel$i1, bf), i_to_x(mel$i2, bf)))
#' ys <- t(cbind(i_to_y(mel$i1, bf), i_to_y(mel$i2, bf)))
#'
#' # Plot maximum error for each cell and a line to the cell where the distance
#' # had that error.
#' plot(rasterize_distr(as.numeric(max_error), bf))
#' title(main = "Max proportional error")
#' matlines(xs, ys, col = rgb(0, 0, 0, .25), lty = 1, lwd = 1)
#' mtext(paste0(
#'   "lines connect cells to the cell which have greatest",
#'   " proportional error in distance"
#' ), line = 0, cex = 0.7)
great_circle_distances <- function(bf) {
  i <- seq_len(n_active(bf))
  xy <- as.data.frame(i_to_xy(i, bf))

  # convert to sf
  pts <- sf::st_as_sf(xy, coords = c("x", "y"))
  sf::st_crs(pts) <- crs(bf)

  # transform to wgs84
  pts <- sf::st_transform(pts, crs = sf::st_crs("EPSG:4326"))

  # Convert to s2 object
  geo <- s2::as_s2_geography(pts)

  # calculate great circle distance matrix (n x n), meters
  gs <- s2::s2_distance_matrix(geo, geo) # great circle

  # convert to km
  return(gs / 1000)
}


#' Calculate the great circle distance based on longitude and latitude
#'
#' @description Calculate the great circle distance
#'
#' @param lat1 latitude of point 1
#' @param lon1 longitude of point 1
#' @param lat2 latitude of point 2
#' @param lon2 longitude of point 2
#' @return the great circle distance
great_circle_distance_lonlat_input <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180 # Conversion factor for degrees to radians
  lat1 <- lat1 * rad
  lon1 <- lon1 * rad
  lat2 <- lat2 * rad
  lon2 <- lon2 * rad

  dlat <- lat2 - lat1
  dlon <- lon2 - lon1

  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  R <- 6371 # Earth's radius in kilometers
  R * c
}
