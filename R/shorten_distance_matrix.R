#' shorten and expand distance matrices
#'
#' These functions are to facilitate storing just the non-duplicated elements
#' of a distace matrix in a vector to and hdf5 file.
#' Since the distance matrix is symetrical and has zero's on the diagonal all
#' the distances can be stored in a vector that contains fewer than half the
#' values in the full matrix.
#'
#' `shorten_distance_matrix()` extracts the lower triangle in column major
#' order from a distance matrix.
#'
#' `expand_distance_matrix()` will reassemble the full distance matrix
#'
#' The purpose is to halve the number of values being stored in the hdf5 file.
#'
#'
#' For a 5 x 5 distance matrix it would be values corresponding with the
#'  numbered cells below.
#'
#'  |   | r1| r2| r3| r4| r5|
#'  |:--|--:|--:|--:|--:|--:|
#'  |c1 | NA| NA| NA| NA| NA|
#'  |c2 |  1| NA| NA| NA| NA|
#'  |c3 |  2|  5| NA| NA| NA|
#'  |c4 |  3|  6|  8| NA| NA|
#'  |c5 |  4|  7|  9| 10| NA|
#'
#'  Due to the symmetry of a distance matrix that's equivalent to the upper
#'  triangle in row major order - which is probably how it will be treated
#'  in python.
#'
#'  |   | c1| c2| c3| c4| c5|
#'  |:--|--:|--:|--:|--:|--:|
#'  |r1 | NA|  1|  2|  3|  4|
#'  |r2 | NA| NA|  5|  6|  7|
#'  |r3 | NA| NA| NA|  8|  9|
#'  |r4 | NA| NA| NA| NA| 10|
#'  |r5 | NA| NA| NA| NA| NA|
#'
#' @aliases expand_distance_matrix
#' @rdname shorten_distance_matrix
#' @param dm a distance matrix, a symetrical n x n, matrix which contains all
#' the distances among n locations.
#' @param values The values from a lower triangle of a distance matrix in row
#' major order.
#' @return `shorten_distance_matrix()` returns a vector of values.
#' `expand_distance_matrix()` returns the full matrix given that vector.
#' @keywords internal
#' @examples
#' \dontrun{
#' x <- runif(5, 1, 100)
#' y <- runif(5, 1, 100)
#' dm <- as.matrix(dist(cbind(x, y)))
#' a <- shorten_distance_matrix(dm)
#' dm2 <- expand_distance_matrix(a)
#' all.equal(dm, dm2, check.attributes = FALSE)
#' }
shorten_distance_matrix <- function(dm) {
  stopifnot(isTRUE(all.equal(t(dm), dm)))
  stopifnot(all(diag(dm) == 0))
  return(dm[lower.tri(dm)])
}

#' @rdname shorten_distance_matrix
#' @keywords internal
expand_distance_matrix <- function(vals) {
  n <- (1 + sqrt(1 + 8 * length(vals))) / 2
  m <- matrix(NA, n, n)
  m[lower.tri(m)] <- vals
  m[!lower.tri(m)] <- 0
  m <- m + t(m)
  return(m)
}
