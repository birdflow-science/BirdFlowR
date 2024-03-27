#' Estimate net bird movement
#'
#' `calc_flux()` estimates the proportion of the species that passes through
#' a set of points during each transition in a BirdFlow model.
#'
#' @param bf A BirdFlow model
#' @param points A set of points to calculate movement through. If `points` is
#' `NULL` they will default to the BirdFlow model cells that are either active
#' or fall between two active cells. Should be a data frame with `x` and  `y`
#' columns containing point coordinates in [crs(bf)][crs()].
#' @param radius The radius in meters around the points, used to determine if
#' a path is passing through the point.
#' @param n_directions The number of directional bins to use for recording
#' movement direction. Must be either `1` indicating no direction information
#' or an even number.
#' @param format The format to return the results in one of:
#' \describe{
#' \item{`"points"`}{Returns a list with `net_movement` a matrix or array of
#'  movement, and `points` a data frame of either the input `points` or the
#'  point locations }
#' \item{`"dataframe"`}{Returns a "long" data frame with columns:
#' * `x` and `y` coordinates of the points.
#' * `transition` Transition code.
#' * `movement` The total abundance moving through the point in the transition.
#' * `date` The date associated with the transition, will be at the midpoint
#'          between timesteps.
#'
#'  }
#' \item{`"SpatRaster"`}{Returns a `terra::SpatRaster` with layers for each
#' transition.}
#'}
#'
#' @return See `format` argument.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' bf <- BirdFlowModels::amewoo
#' flux <- calc_flux(bf)
#'
#' plot_flux(flux, bf)
#'
#' animate_flux(flux, bf)
#' }
#'
calc_flux <- function(bf, points = NULL, radius = NULL, n_directions = 1,
                      format = NULL) {

  if (n_directions != 1)
    stop("Only one directional flux is supported at the moment.")

  if (is.null(format)) {
    if (is.null(points)) {
      format <- "dataframe"
    } else {
      format <- "points"
    }
  }

  format <- tolower(format)
  stopifnot(format %in% c("points", "spatraster", "dataframe"))


  result <- is_between(bf, points, radius, n_directions)
  between <- result$between
  points <- result$points

  timesteps <- lookup_timestep_sequence(bf)
  transitions <- lookup_transitions(bf)
  marginals <- gsub("^T", "M", transitions)

  # Empty result matrix, rows are points from between columns are timesteps
  net_movement <-
    matrix(NA_real_,
           nrow = dim(between)[3],
           ncol = n_transitions(bf),
           dimnames = c(dimnames(between)[3],
                        list(transition =  transitions)))


  n_pts <- dim(between)[3]
  for (i in seq_along(marginals)) {
    from <- timesteps[i]
    to <- timesteps[i + 1]
    mar <- get_marginal(bf, marginals[i])
    fdm <- get_dynamic_mask(bf, from)
    tdm <- get_dynamic_mask(bf, to)

    # subset betweenness (sb) to conform to this marginals dynamic masks
    sb <- between[fdm, tdm, ]
    stopifnot(all(dim(sb)[1:2] == dim(mar)))  # verify

    for (j in seq_len(n_pts)){
      net_movement[j, i] <- sum(mar[sb[, , j]])
    }
  }


  if (format == "points") {
    return(list(net_movement, points))
  }

  if (format == "spatraster") {
    raster <- array(data = NA, dim = c(nrow(bf), ncol(bf), ncol(net_movement)))
    dimnames(raster) <- list(row = NULL, col = NULL,
                            transition = colnames(net_movement))
    rc <- cbind(y_to_row(points$y, bf), x_to_col(points$x, bf))
    colnames(rc) <- c("row", "col")
    for (i in seq_along(marginals)) {
      # The line below uses matrix indexing of an array where each
      # row of the matrix defines a particular cell by treating the
      # values in that row as an index on the dimensions of the array
      # eg 1, 10, 40 will index rast[1, 10, 30]
      rcl <- cbind(rc, loc = i)
      raster[rcl] <- net_movement[, i]
    }
    r <- terra::rast(raster, extent = bf$geom$ext, crs = bf$geom$crs)
    names(r) <- transitions
    return(r)
  }

  if (format == "dataframe") {
    wide <- cbind(as.data.frame(points)[, c("x", "y")],
                net_movement)
    long <- tidyr::pivot_longer(wide, cols = setdiff(names(wide), c("x", "y")),
                                names_to = "transition", values_to = "movement")

    long$date <- as.character(lookup_date(long$transition, bf))

    return(as.data.frame(long))

  }

  stop(format, "is not a recoginized format.") # shouldn't ever get here
}
