
#' Assign weights to points based on how close they are to the planar
#' (Euclidean) line associated with each transition
#'
#' This internal function is used to create a betweenness weights array
#' with dimensions [n_active(bf)](n_active()),
#' [n_active(bf)](n_active()), and `length(points)`.
#' The first two dimensions represent from and to cells of possible connections
#' between  pairs of locations within the BirdFlow model and both have
#' `n_active()` elements.
#' The third dimension represents reference points that might be between each
#' connection.
#' Cell values are the weight to use when adding the transition probabilities
#' to the reference point BMTR
#'
#' If `points`  are `NULL` they default to the center of all cells within
#' the BirdFlow extent that fall between any active cells.
#' This includes all cell centers within a convex hull
#' (in the model's native, projected CRS) around the active cells in `bf`
#' and thus is almost always more than just the active cells.
#'
#' Unlike [calc_spherical_detection_rate()], this projects points onto the
#' line segment connecting each pair of active cells directly in the model's
#' native (planar) CRS, using vectorized matrix algebra
#' ([project_points_simple()]) rather than great-circle math. This is
#' substantially faster and is the recommended continuous detection method.
#'
#' @param bf A BirdFlow model
#' @param points The points to evaluate betweenness on. If NULL the cell
#' centers of all the raster cells within the BirdFlow model that are between
#' active cells in the model will be used. This is calculated by comparing the
#' cell centers to a buffered convex hull around the active cell centers.
#' @param radius The probability density along a transect perpendicular to
#' the line and intersecting the point is summed over the interval within
#' `radius` meters of the point. `radius` defaults to half the cell
#' size (`mean(res(bf))/2`).
#' @param n_directions The number of (equally spaced) directional bins to
#' classify bearings into.  Currently only `1` is supported.
#' @param skip_unconnected If `TRUE` then only connections that exist in `bf`
#' will be evaluated, and between matrix will erroneously indicate that
#' the weights associated with transitions that aren't used is always 0.
#' The resulting array can still be used with the model it was built for because
#' those missing connections would always have zero probability.
#' @param batch_size controls the number of movement lines that are processed
#' at a time. A smaller
#' `batch_size` will conserve memory at a slight performance cost.  The number
#' of batches will be less than or equal to `n_active(bf)^2 / batch_size`.
#' @param check_radius If  `TRUE` an error will be thrown if the radius
#' is not between the resolution and 1/4 the resolution of `bf`. Outside of
#' that range the algorithm is likely to yield distorted results.
#' `0.5 * mean(res(bf))` is the default, and recommended radius.
#' @param ... Additional arguments forwarded to [calc_dist_weights()],
#' allowing the spread kernel (`kernel`) and its hyperparameters
#' (`gamma`, `kl`, `s1`) to be tuned.
#' @return A list with:
#' \item{between}{An array with dimensions representing the
#' "from" location, the "to" location, and the `points`. Cells are weights and
#' will be non-zero if the radius around the point intersects 1.96 standard
#' deviations of the normal distribution of probabilities around the line.}
#'  \item{points}{A data,frame of points that define the third dimension
#'  in `between`.  It is identical to the input `points` if they are not `NULL`.
#'  Otherwise it will be a data frame with columns `x`, `y`, and `i`
#'  corresponding to the third dimension in `between`.
#'  `i` will be `NA` for points that are not within the mask but
#'  fall between active cells.}
#'  \item{radius}{The radius of the circle in meters.}
#' @seealso [is_between()], [calc_spherical_detection_rate()], and
#' [calc_bmtr()]
#' @keywords internal
calc_euclidean_detection_rate <- function(bf, points = NULL, radius = NULL,
                                          n_directions = 1,
                                          skip_unconnected = TRUE,
                                          batch_size = 1e5,
                                          check_radius = TRUE, ...) {
  bf_msg("Generating between weights.\n")

  if (!requireNamespace("SparseArray", quietly = TRUE)) {
    stop("The SparseArray package is required to use is_between(). ",
         "Please install it prior to calling this function.")
  }

  if (!n_directions == 1) {
    stop("Currently only one direction is supported.")
  }
  # Dimensions
  # 1 from location  n_active()
  # 2 to location    n_active()
  # 3 points  (if NULL use cell centers)
  # 4 (pending) n_direction, directional bins.

  if (is.null(radius)) {
    radius <- mean(res(bf)) / 2
  } else if (check_radius) {
    # Analysis of the effect of changing radius is in test-calc_bmtr.R
    radius_cells <- radius / mean(res(bf)) # radius converted to cells
    if (radius_cells <= 0.25 || radius_cells >= 1) {
      stop("radius should be less than the resolution and more than 1/4 the ",
           "resolution or BMTR is likely to be biased. ",
           "Set check_radius to FALSE to ignore this advice.")
    }
  }

  # Hull buffer is only used for selecting default points, not for the
  # detection radius used in the weighting below.
  hull_buffer <- 2 * mean(res(bf))

  if (is.null(points)) {
    bf_msg("  Creating points\n")
    # Create points at all cell centers in the rectangular raster
    points <- rasterize_distr(get_distr(bf, 1), bf, format = "dataframe")
    points <- points[, c("x", "y", "i")]
    active <- points[!is.na(points$i), , drop = FALSE]

    bf_msg("  Preparing sf objects\n")
    # Convert active and points to sf objects
    active_sf <- active |>
      sf::st_as_sf(coords = c("x", "y"), crs = crs(bf))
    points_sf <- points |>
      sf::st_as_sf(coords = c("x", "y"), crs = crs(bf))

    bf_msg("  Creating buffered convex hull\n")
    # Make a buffered convex hull around the active cells
    hull <- sf::st_union(active_sf) |>
      sf::st_convex_hull() |>
      sf::st_buffer(dist = units::set_units(hull_buffer, "m"))

    bf_msg("  Selecting points inside hull\n")
    # Keep the points inside the hull
    sv <- points_sf |>
      sf::st_intersects(y = hull, sparse = FALSE) |>
      as.vector()

    if (FALSE) {
      # visualize
      plot(hull)
      plot(points, col = "grey", add = TRUE)
      plot(points[sv, ], col = "black", add = TRUE)
      plot(active, col = "red", add = TRUE)
    }

    # Subset to the cells that are active or between other active cells
    # Note this is in back in original projection
    points <- points[sv, , drop = FALSE]
  }

  bf_msg("  Initializing arrays. \n")

  # Initialize sparse array (All FALSE) to hold betweenness
  # This is a hack because there's no creation method that allows setting a
  # dimension, but I can make an empty "random" array.
  between <- SparseArray::randomSparseArray(
    dim = c(n_active(bf), n_active(bf), nrow(points)),
    density = 0) != 0
  dimnames(between) <- list(from = paste0("F_", seq_len(n_active(bf))),
                            to = paste0("T_", seq_len(n_active(bf))),
                            loc = paste0("L_", seq_len(nrow(points))))


  # Generate table of active cell x, y, and i (in birdflow CRS)
  active <- i_to_xy(seq_len(n_active(bf)), bf)
  active$i <- seq_len(n_active(bf))

  # All possible pairs of active points
  all_pairs <- expand.grid(from = seq_len(n_active(bf)),
                           to = seq_len(n_active(bf)))
  all_pairs$id <- NA_character_
  sv <- all_pairs$from < all_pairs$to
  all_pairs$id[sv] <- paste(all_pairs$from[sv], "-", all_pairs$to[sv])
  all_pairs$id[!sv] <- paste(all_pairs$to[!sv], "-", all_pairs$from[!sv])

  # Unique pairs of points disregarding order
  pairs <- all_pairs[!duplicated(all_pairs$id), , drop = FALSE]

  # Drop self, self pairs.
  # They represent stop overs or seasonal residence, not migratory movement
  pairs <- pairs[pairs$from != pairs$to, , drop = FALSE]

  # Drop pairs that aren't ever connected
  if (skip_unconnected) {
    # Create matrix indicating which active cells are connected to
    # each other via a non-zero marginal at any timestep
    # With sparse models this will eliminate a lot of connections
    # With non-sparse models it will still eliminate some
    #  due to dynamic masking.
    ever_connected <- matrix(FALSE, n_active(bf), n_active(bf))
    dm <- get_dynamic_mask(bf)
    mi <- bf$marginals$index
    mi <- mi[mi$direction == "forward", ]

    for (i in seq_len(nrow(mi))) {
      from_dm <- dm[, mi$from[i]]
      to_dm <- dm[, mi$to[i]]
      marg <- get_marginal(bf, mi$marginal[i])
      ever_connected[from_dm, to_dm] <- as.matrix(marg != 0)
    }
    ever_connected <- ever_connected | t(ever_connected) # backwards counts

    connected_pairs <- data.frame(from = row(ever_connected)[ever_connected],
                                  to = col(ever_connected)[ever_connected])
    connected_pairs$id <- with(connected_pairs, paste(from, "-", to))

    pairs <- pairs[pairs$id %in% connected_pairs$id, , drop = FALSE]

  }

  # Project points onto line segments in batches to bound memory use of the
  # (n points) x (m lines) projection matrices.
  p_mat <- as.matrix(points[, c("x", "y")])
  m <- nrow(p_mat)   # number of points
  n_total <- nrow(pairs)
  batch_ids <- split(seq_len(n_total),
                     ceiling(seq_len(n_total) / batch_size))

  for (b in seq_along(batch_ids)) {
    idx <- batch_ids[[b]]
    pairs_batch <- pairs[idx, , drop = FALSE]

    s_mat <- as.matrix(active[pairs_batch$from, c("x", "y")])
    e_mat <- as.matrix(active[pairs_batch$to, c("x", "y")])

    n <- nrow(s_mat)

    v_mat <- e_mat - s_mat
    vv <- rowSums(v_mat^2)

    projection <- project_points_simple(s = s_mat, e = e_mat, p = p_mat,
                                        m = m, n = n, v = v_mat, vv = vv)
    dist_to_line <- as.vector(projection$dist_to_line)
    dist_along_line <- as.vector(projection$dist_along)
    line_lengths <- rep(sqrt(vv), each = m)

    line_index <- rep(seq_len(n), each = m)
    point_index <- rep(seq_len(m), times = n)

    valid <- !is.na(dist_along_line) & dist_along_line > 0 &
      dist_along_line < line_lengths

    weights <- calc_dist_weights(dist_to_line[valid],
                                 dist_along_line[valid],
                                 line_lengths[valid],
                                 res_m = mean(res(bf)),
                                 radius_m = radius,
                                 ...)

    add <- cbind(pairs_batch$from[line_index[valid]],
                pairs_batch$to[line_index[valid]],
                point_index[valid],
                weights)
    add <- add[add[, 4] != 0, , drop = FALSE]

    between[add[, c(1, 2, 3)]] <- add[, 4]
    between[add[, c(2, 1, 3)]] <- add[, 4]

    bf_msg("    ", round(b / length(batch_ids) * 100, 2), "%\n")
  }

  return(list(between = between, points = points, radius = radius))
}

#' Project points onto line segments (Euclidean/planar geometry)
#'
#' Vectorized computation of the distance from each point in `P` to each
#' line segment defined by `S` (starts) and `E` (ends), and the distance
#' along the segment to the projection of the point.
#'
#' @param s An n x 2 matrix of segment start coordinates.
#' @param e An n x 2 matrix of segment end coordinates.
#' @param p An m x 2 matrix of point coordinates.
#' @param m Number of points (`nrow(p)`).
#' @param n Number of line segments (`nrow(s)`).
#' @param v `e - s`, precomputed segment vectors.
#' @param vv `rowSums(v^2)`, precomputed squared segment lengths.
#' @param clamp If `TRUE` constrain the projected fraction to `[0, 1]`. If
#' `FALSE` (default) projections falling outside the segment are set to `NA`.
#' @param tol Segments with squared length at or below this threshold are
#' treated as having zero length (projected fraction set to 0).
#' @return A list with `dist_to_line` and `dist_along`, each a vector of
#' length `m * n` (point index varying fastest).
#' @keywords internal
project_points_simple <- function(s, e, p, m, n, v, vv,
                                  clamp = FALSE, tol = 0) {

  # 1. Vectorized dot products (m x n matrix) giving the fraction of each
  # line's length at which each point projects onto that line.
  t_frac <- (p %*% t(v) - rep(rowSums(s * v), each = m)) / rep(vv, each = m)

  # Zero-length lines project everything to their start.
  t_frac[, vv <= tol] <- 0

  if (clamp) {
    t_frac <- pmax(0, pmin(1, t_frac))
  } else {
    t_frac[t_frac < 0 | t_frac > 1] <- NA
  }

  # Coordinates of the projected point for every (point, line) combination.
  projx <- rep(s[, 1], each = m) + t_frac * rep(v[, 1], each = m)
  projy <- rep(s[, 2], each = m) + t_frac * rep(v[, 2], each = m)

  dist_to_line <- sqrt((rep(p[, 1], times = n) - projx)^2 +
                         (rep(p[, 2], times = n) - projy)^2)

  dist_along <- t_frac * rep(sqrt(vv), each = m)

  list(dist_to_line = dist_to_line, dist_along = dist_along)
}
