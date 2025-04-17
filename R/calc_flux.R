#' Estimate bird flux
#'
#' `calc_flux()` estimates the proportion of the species that passes near
#' a set of points during each transition in a BirdFlow model.
#'
#' @section Units:
#'
#' The total relative abundance passing through the circle centered on the point
#'  is divided by the diameter of the circle in kilometers.  The units of the
#'  returned value is therefore roughly the proportion
#' (\eqn{P}) of the
#' species's population that is expected to pass through each km
#' of a line oriented perpendicular to the movement at each point:
#' \eqn{\frac{P}{km \cdot week}}
#'
#' Multiplying the result by the total population would yield:
#' \eqn{\frac{birds}{km \cdot week}}
#'
#' @section Limitations:
#'
#' `calc_flux()` makes the incorrect simplifying assumption
#'  that birds follow the shortest (great circle) path
#' between the center of the the source and destination raster cells.  Caution
#' should be used when interpreting the results especially around
#' major geographic features such as coasts, large lakes, mountain ranges, and
#' ecological system boundaries that might result in non-linear migration paths.
#'
#' `calc_flux()` assumes that a line passes by a point if any part of the line
#' is within the radius of the point.  This assumption breaks down if the
#' radius is much larger than the movement lengths as points that are ahead of
#' the line may still be within a radius of the line.  In the extreme a large
#' enough radius on a point outside of the entire range will capture all the
#' movement.  This isn't a problem with
#' the default points and radius as the points ahead of the line will never be
#' within the radius.
#'
#' The default points for `calc_flux()` are aligned with the cell centers as
#' are the movement lines. This alignment means that a very small radius will
#' result in an overestimate of flux. The default value of half the cell size
#' is sufficient for this not to be a problem, as we are capturing and
#' standardizing the units based on the entire cell area that that point
#' represents.
#'
#'
#' @param bf A BirdFlow model
#' @param points A set of points to calculate movement through. If `points` is
#' `NULL` they will default to the BirdFlow model cells that are either active
#' or fall between two active cells. Otherwise a data frame with `x` and  `y`
#' columns containing point coordinates in [crs(bf)][crs()].
#' @param radius The radius in meters around the points used to assess whether
#' a movement line passes by (or through) the point. If a point is farther than
#' `radius` from a great circle line between two cells centers then it is not
#' between them.
#' @param n_directions The number of directional bins to use for recording
#' movement direction. Must be either `1` indicating no direction information
#' or an even number. This is a placeholder, currently only `1` is supported.
#' @param format The format to return the results in one of:
#' \describe{
#' \item{`"points"`}{Returns a list with `flux` a matrix or array of
#'  flux values, and `points` a data frame of either the input `points` or the
#'  default cell center derived points.}
#' \item{`"dataframe"`}{Returns a "long" data frame with columns:
#' * `x` and `y` coordinates of the points.
#' * `transition` Transition code.
#' * `flux` The flux at the point. See "Units" below  .
#' * `date` The date associated with the transition, will be at the midpoint
#'          between timesteps.
#'
#'  }
#' \item{`"SpatRaster"`}{Returns a `terra::SpatRaster` with layers for each
#' transition.}
#'}
#' @inheritParams is_between
#' @param weighted If `FALSE` use the original and quicker version of flux
#' that sums all the marginal probability for transitions that pass within a
#' fixed distance of the point.  If `TRUE` assign a weight to the point and
#' transition combo that then is multiplied by the marginal probability before
#' summing.  This argument is experimental but the default value is identical
#' to the old version. The argument name and behavior when set to `TRUE` may
#' change.
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
                      format = NULL, batch_size = 5e5, check_radius = TRUE,
                      weighted = FALSE) {


  if (!requireNamespace("SparseArray", quietly = TRUE)) {
    stop("The SparseArray package is required to use calc_flux(). ",
         "Please install it prior to calling this function.")
  }

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

  # The only difference between is_between() and weight_between() return formats
  # is in the "between" component.  In the first it's logical (TRUE is between)
  # in the second it is a weight between 0 and 1 (non zero indicates some
  # level of betweeness".
  if (weighted) {
    result <- weight_between(bf, points, radius, n_directions)
  } else {
    result <- is_between(bf, points, radius, n_directions)
  }

  between <- result$between
  points <- result$points
  radius_km <- result$radius / 1000

  bf_msg("Calculating Flux\n")

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

  bf_msg("  Processing Marginals\n")
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
      # This is a little hacky the marginal is a sparse Matrix::Matrix
      # or a standard Matrix (depending on whether bf is sparse)
      # sb and between are  SparseArray::SparseArray
      # SparseArray doesn't support logical sub-setting ("yet"),
      # Matrix does. The solution is to convert the selection
      # to a standard matrix or a sparse Matrix. The first is empirically a
      # lot faster
      sel <- as.matrix(sb[, , j]) # either logical or weights
      net_movement[j, i] <- sum(mar * sel)
    }
  }

  bf_msg("  Formatting flux\n")
  # Standardize to P of population to pass through KM of transect in a week
  net_movement <- net_movement / (radius_km * 2)

  if (format == "points") {
    return(list(flux = net_movement, point = points))
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
                                names_to = "transition", values_to = "flux")

    long$date <- as.character(lookup_date(long$transition, bf))

    return(as.data.frame(long))

  }

  stop(format, "is not a recoginized format.") # shouldn't ever get here
}
