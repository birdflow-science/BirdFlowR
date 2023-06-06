#'Sample locations from a distribution
#'
#'Probabilistically sample one or more locations from a set of distributions.
#'
#'@param distr a vector representing a single distribution; a matrix
#' representing one distribution per column, or (unlikely) an  array in which
#' the first dimension represents locations. The values in `distr` are
#' treated as the relative probability of the species being in each position.
#'@param n Only used if `distr` is a vector representing a single model state,
#'  in which case that model state will be sampled `n` times to generate a
#'  matrix representing `n` sampled locations from that distribution.
#'@param bf A BirdFlow object, required if format is `"latlon"` or `"xy"`,
#'  optional otherwise.
#'@param format One of `"distr"` (the default), `"xy"`, `"latlon`, or `"i"`
#'  indicating what format the sample should be returned in.
#'@return One or more location samples from the distributions in `distr` the
#'format changes with the value of`format`:
#'\item{`distr`}{Default. Return an object with the same dimensions as
#'   `distr` in which all the weight in each distribution in
#'  `distr` is assigned to a single location containing a 1 and
#'  remaining locations have 0's.}
#'\item{`xy`}{`x` and `y` coordinates of locations, usually as data frame but
#' with 3D input it will be an array.}
#'\item{`latlon`}{return latitude (`lat`) and longitude (`lon`) coordinates in
#' WGS 1984 for the sampled locations, usually as a data frame but with 3D
#' input it will be an array. }
#'\item{`i`}{return location index for the sampled locations}
#'
#'
#'@export
#'
#' @examples
#'
#' bf <- BirdFlowModels::amewoo
#' d <- get_distr(bf, 5)
#'
#' # default format "distr" returns an object similar to the input
#' # in which all the weight for each distribution has been concentrated
#' # in a single location
#' one_hot <- sample_distr(d)
#' all(one_hot %in% c(0, 1))
#' sum(one_hot)
#'
#' # Sample 10 times from a single distribution and return x and y coordinates.
#' xy <- sample_distr(d,  10, format = "xy", bf = bf)
#'
sample_distr <- function(distr, n = 1, bf, format = "distr") {

  # sample_distr() is called with the default format repeatedly
  # inside of a loop while routing, and performance is optimized for that
  # usage.

  select_one <- function(x) {
    # Takes a probability vector and randomly converts one value to 1
    # using the original values as probabilities
    i <- sample(seq_along(x), size = 1, prob = x)
    r <- rep(0, length(x))
    r[i] <- 1
    return(r)
  }

  distr <- as.array(distr)
  distr_dim <- dim(distr)
  distr_ndim <- length(distr_dim)
  distr_names <- dimnames(distr)

  if (distr_ndim == 1 && n > 1) {
    # repeat values in distr, n times while converting to matrix
    distr <- array(data = distr, dim = c(length(distr), n))
    distr_ndim <- 2
  }

  # Select one from each state
  if (distr_ndim == 1) {
    distr <- select_one(distr)
  } else {
    distr <- apply(distr, 2:distr_ndim, select_one)
  }

  if (format == "distr")
    return(distr)


  if (distr_ndim == 1) {
    ind <- which(as.logical(distr))
  } else {
    ind <- apply(distr, 2:distr_ndim, function(x) which(as.logical(x)))
  }

  if (format == "i")
    return(ind)

  if (missing(bf) || !inherits(bf, "BirdFlow"))
    stop('bf argument must be a BirdFlow object for formats "xy" and "latlon"')

  # Convert to x and y coordinates. This drops dimensions
  x <- i_to_x(ind, bf)
  y <- i_to_y(ind, bf)

  if (format == "xy") {

    if (distr_ndim %in% c(1, 2)) {
      return(data.frame(x, y))
    }

    # For 3+ dimension input
    merged_dim <- c(distr_dim[-1], 2)
    merged_dimnames <- c(distr_names[-1], list(coords = c("x", "y")))
    xy <- array(data = c(x, y),
                dim = merged_dim,
                dimnames = merged_dimnames)
    return(xy)

  }

  if (format == "latlon") {
    latlon <- xy_to_latlon(x = x, y = y, bf)

    if (distr_ndim %in% c(1, 2))
      return(latlon)

    # For 3+ dimensional input
    lat <- latlon[, 1]
    lon <- latlon[, 2]
    merged_dim <- c(distr_dim[-1], 2)
    merged_dimnames <- c(distr_names[-1], list(coords = c("lat", "lon")))
    latlon <- xy_to_latlon(x = x, y = y, bf)
    lat <- latlon[, 1]
    lon <- latlon[, 2]
    latlon <- array(data = c(lat, lon),
                    dim = merged_dim,
                    dimnames = merged_dimnames)
    return(latlon)
  }
  stop('format must be one of "distr", "i", "xy", or "latlon"')

}
