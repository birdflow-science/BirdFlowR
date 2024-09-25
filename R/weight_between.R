
if (FALSE) {
  bf <- BirdFlowModels::amewoo
  points <- NULL
  radius <- NULL
  between <- is_between(bf)
}

#' Assign weights to points based on how close they are to the great circle
#' line associated with each transition
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
#' to the reference point flux.
#'
#' If `points`  are `NULL` they default to the center of all cells within
#' the BirdFlow extent that fall between any active cells.
#' This includes all cell centers within a convex hull
#' (in spherical coordinates) around the active cells in `bf` and thus is
#' almost always more than just the active cells.
#'
#' `weight_between()` and `is_between()` should only differ slightly in
#' their results when calculated on the same model.
#'
#' 1, The number of included reference points may differ.
#' 2. The betweeness array will be real weights from 0 to 1 with
#' `weight_between()` and logical with `is_between()`
#'
#'
#' @param bf A BirdFlow model
#' @param points The points to evaluate betweenness on. If NULL the cell
#' centers of all the raster cells within the BirdFlow model that are between
#' active cells in the model will be used. This is calculated by comparing the
#' cell centers to a buffered convex hull around the active cell centers.
#' @param radius The probability density along a transect perpendicular to
#' the line and intersecting the point is summed over the inteval within
#' `radius` meters of the point. `radius` defaults to half the cell
#' size (`mean(res(bf))/2`).
#' @param n_direction The number of (equally spaced) directional bins to
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
#' `mean(res(bf)) / 2` is the default, and recommended radius.
#' @return A list with:
#' \item{between}{An array with dimensions representing the
#' "from" location, the "to" location, and the `points`. Cells are weights and
#' will be non-zero if the radius around the point intersects 1.96 standard
#' deviations of the normal distribution of probabilities around the great
#' circle line.}
#'  \item{points}{A data,frame of points that define the third dimension
#'  in `between`.  It is identical to the input `points` if they are not `NULL`.
#'  Otherwise it will be a data frame with columns `x`, `y`, and `i`
#'  corresponding to the third dimension in `between`.
#'  `i` will be `NA` for points that are not within the mask but
#'  fall between active cells.}
#'  \item{radius}{The radius of the circle in meters.}
#'  @seealso [is_between()] and [calc_flux()]
#' @keywords internal
weight_between <- function(bf, weight_fun = NULL, points = NULL, radius = NULL,
                           n_directions = 1, skip_unconnected = TRUE,
                           batch_size = 1e5, ...) {

  bf_msg("Generating between weigts.\n")

  if (!requireNamespace("SparseArray", quietly = TRUE)) {
    stop("The SparseArray package is required to use is_between(). ",
         "Please install it prior to calling this function.")
  }

  # Force use of s2
  o_use_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(o_use_s2))
  sf::sf_use_s2(TRUE)

  if (!n_directions == 1) {
    stop("Currently only one direction is supported.")
  }
  # Dimensions
  # 1 from location  n_active()
  # 2 to location    n_active()
  # 3 points  (if NULL use cell centers)
  # 4 (pending) n_direction, directional bins.

  # Radius is just used for buffering the convex hull
  # I'm not sure what the buffer should be so am just setting
  # it to the cell dimension.  I may have to loop back later and figure
  # something else out.
  radius <- mean(res(bf))

  if (is.null(points)) {
    bf_msg("  Creating points\n")


    # Create points at all cell centers in the rectangular raster
    points <- rasterize_distr(get_distr(bf, 1), bf, format = "dataframe")
    points <- points[, c("x", "y", "i")]
    active <- points[!is.na(points$i), , drop = FALSE]

    bf_msg("  Transforming to spherical coordinates\n")

    # Convert to lat long (spherical) coordinates
    active_sph <- sf::st_as_sf(active, coords = c("x", "y"), crs = crs(bf)) |>
      sf::st_transform(crs = "EPSG:4326")
    points_sph <- points |>
      sf::st_as_sf(coords = c("x", "y"), crs = crs(bf)) |>
      sf::st_transform(crs = "EPSG:4326")

    # Make a buffered convex hull around the active cells in lat lon
    hull <- sf::st_union(active_sph) |>
      sf::st_convex_hull() |>
      sf::st_buffer(hull, dist = units::set_units(radius, "m"))

    # Selection vector for points that are active or between active cells
    sv <- points_sph |>
      sf::st_intersects(y = hull, sparse = FALSE) |>
      as.vector()

    if (FALSE) {
      # visualize
      plot(hull)
      plot(points_sph, col = "grey", add = TRUE)
      plot(points_sph[sv, ], col = "black", add = TRUE)
      plot(active_sph, col = "red", add = TRUE)
    }

    # Subset to the cells that are active or between other active cells
    # Note this is in back in original projection
    points <- points[sv, , drop = FALSE]
  }

  bf_msg("  Preparing points\n")


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

  # Convert points and active to sf objects and spherical coordinates (lat lon)
  points_sph <- points |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs(bf)) |>
    sf::st_transform(crs = "EPSG:4326")
  active_sph <- active |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs(bf)) |>
    sf::st_transform(crs = "EPSG:4326")

  # Convert points to s2 object
  points_s2 <- s2::as_s2_geography(points_sph)


  # Spherical coordinates of active cells
  a_coords <- sf::st_coordinates(active_sph)
  colnames(a_coords) <- c("x", "y")

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

    for (i in  seq_len(nrow(mi))) {
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

  # Add batch number to pairs of points
  pairs$batch <- rep(seq_len(nrow(pairs)),
                     each = batch_size,
                     length.out = nrow(pairs))
  n_batch <- max(pairs$batch)

  # Process pairs of active points in batches - converting to great circle
  # lines connecting each pair and then determining which evaluation points
  # are within the distance
  bf_msg("  Processing points in ", n_batch, " batches \n")
  for (batch in seq_len(n_batch)) {
    b_pairs <- pairs[pairs$batch == batch, , drop = FALSE]
    # Lines connecting pairs of points
    lines <- vector(mode = "list", length  = nrow(b_pairs))
    for (i in seq_along(lines)) {
      lines[[i]] <-
        sf::st_linestring(a_coords[c(b_pairs$from[i], b_pairs$to[i]), ])
    }

    # Convert to simple features column and add CRS
    lines_sfc <- sf::st_sfc(lines, crs = "EPSG:4326")
    lines_s2 <- s2::as_s2_geography(lines_sfc)

    # For each line-point combination determine
    # Line length "T"
    # Distance along the line to the point project onto the line, "t"
    # Distance from the point to the line "D"
    line_lengths <- s2::s2_length(lines_s2) #  meters

    # Setup for element wise vectorized calculations by repeating each
    # line once for each point.
    n_lines <- length(lines_s2)
    n_points <- length(points_s2)
    lines_rep <- rep(lines_s2, each = n_points)
    points_rep <- rep(points_s2, times = n_lines)

    dist_along_line <- s2::s2_project(lines_rep, points_rep)
    not_end <- dist_along_line != 0 &
      dist_along_line < rep(line_lengths, each = n_points)

    # Just the ones that didn't snap to line ends
    valid_lines <- lines_rep[not_end]
    valid_points <- points_rep[not_end]

    # Assume that points that are snapped to the ends of the lines are
    # before or after the line and so are not valid - we are going to assign
    # them 0 weight.
    valid_dist_along_line <- dist_along_line[not_end]
    valid_dist_to_line <- s2::s2_distance(valid_lines, valid_points)
    valid_line_lengths <- rep(line_lengths, each = n_points)[not_end]
    valid_line_index <- rep(seq_len(n_lines), each = n_points)[not_end]
    valid_point_index <- rep(seq_len(n_points), times = n_lines)[not_end]
    # Placeholder weight fun

    weights <- calc_dist_weights(valid_dist_to_line,
                                 valid_dist_along_line,
                                 valid_line_lengths,
                                 res_m = mean(res(bf)),
                                 radius_m = radius,
                                 method = "m3")


    # Add is a matrix defining the new cells with weight to
    # add to the betweeness array
    # The first three columns are the index in the three dimensional
    # array of a cell (from state, to state, flux point index).
    # The fourth is the weight of the cell.

    add <-  cbind(b_pairs$from[valid_line_index],
                  b_pairs$to[valid_line_index],
                  valid_point_index,
                  weights)


    # Drop rows with zero weight
    add <- add[!add[, 3] == 0, , drop = FALSE]

    # Forward transitions
    between[add[, c(1, 2, 3)]] <- add[, 4]

    # Backwards transitions
    between[add[, c(2, 1, 3)]] <- add[, 4]

    bf_msg("    ", round(batch / n_batch * 100, 2), "%\n")

  } # End batch loop

  if (FALSE) {
    # nolint start: object_usage_linter
    # visualize - for coding or debugging
    plot(hull, reset = FALSE)
    plot(points_sph, col = gray(.8), add = TRUE)
    plot(active_sph, col = gray(.6), add = TRUE)
    i <- 29  # 100

    points(a_coords[c(pairs$from[i], pairs$to[i]),  ], col = "blue", cex = 1.2)
    this_line <- lines_sfc[i] |> # nolint
      sf::st_segmentize(units::set_units(10, "km"))
    plot(this_line, add = TRUE, col = "green")

    # extract weights associated with all points relative to this line
    points_sph$weight <- between[pairs$from[i], pairs$to[i], ]

    # PLot touched points  in green
    #plot(points_sph[ points_sph$weight > 0, ], cex = 2,
    #     col = "green", add = TRUE)


    # Plot touched points colored by weight
    pal <- ebirdst::ebirdst_palettes(n = 256, "weekly")
    col_index <- points_sph$weight / max(points_sph$weight) * 255 + 1
    col_index <-  round(col_index) |> as.integer()
    cols <- pal[col_index]


    plot(points_sph, col = cols, add = TRUE, pch = 19)

    n_legend <- 10
    legend_index <- seq(from = 1, to = 256, length.out = nlegend) |> round()
    legend_colors <- pal[legend_index]
    legend_values <- (legend_index - 1) / 255 * max(points_sph$weight)
    legend_values <- signif(legend_values, 2)
    legend("bottomright", col = legend_colors, legend = legend_values,
           pch = 19, bty = "n", title = "Weight")
    title(main = "Martern 3/2 weights")
    mtext("gamma = 40,000, l = 2,000", side = 3)

    coast <- get_coastline(bf)
    coast <- sf::st_transform(coast, sf::st_crs(lines_sfc))
    plot(coast, add = TRUE)
    #nolint end: object_usage_lintet
  }

  return(list(between = between, points = points, radius = radius))
}
