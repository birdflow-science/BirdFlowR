
if (FALSE) {
  bf <- BirdFlowModels::amewoo
  points <- NULL
  radius <- NULL
  between <- is_between(bf)
}

#' Determine if points are between BirdFlow cells
#'
#' This internal function is used to create a betweenness array with dimensions
#' [n_active(bf)](n_active()), [n_active(bf)](n_active()), and `length(points)`.
#' The first two dimensions from and to cells of possible connections
#' between  pairs of locations within the BirdFlow model and both
#' `n_active()` elements.
#' The third dimension represents `points` that might be between each
#' connection.
#' Cell values are `TRUE` if the point is between the associated model cells.
#' Specifically, if the point is within `radius` meters (along a great circle)
#' of the great circle line connecting the cell centers.
#'
#' If `points` and `radius` are `NULL` they default to the cell radius and the
#' center of all cells within the BirdFlow extent that fall between
#' any active cells. This includes all cell centers within a convex hull
#' (in spherical coordinates) around the active cells in `bf`.
#'
#'
#' @param bf A BirdFlow model
#' @param points The points to evaluate betweenness on. If NULL the cell
#' centers of all the raster cells within the BirdFlow model that are between
#' active cells in the model will be used. This is calculated by comparing the
#' cell centers to a buffered convex hull around the active cell centers.
#' @param radius A point is considered between two locations if it is within
#' `radius` meters (along a great circle) of the great circle line between the
#' locations. `radius` defaults to half the cell size (`mean(res(bf))/2`).
#' @param n_direction The number of (equally spaced) directional bins to
#' classify bearings into.  Currently only `1` is supported.
#' @param skip_unconnected If `TRUE` then only connections that exist in `bf`
#' will be evaluated, and between matrix will erroneously indicate that
#' no points are between locations that are not connected.
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
#' @return A list with:
#' \item{between}{An array with dimensions representing the
#' "from" location, the "to" location, and the `points`. Cells are `TRUE`
#'  if the point is between the associated from and to locations.}
#'  \item{points}{A data,frame of points that define the third dimension
#'  in `between`.  It is identical to the input `points` if they are not `NULL`.
#'  Otherwise it will be a data frame with columns `x`, `y`, and `i`
#'  corresponding to the third dimension in `between`.
#'  `i` will be `NA` for points that are not within the mask but
#'  fall between active cells.}
#'  \item{radius}{The radius of the circle in meters.}
#' @keywords internal
is_between <- function(bf,  points = NULL, radius = NULL, n_directions = 1,
                       skip_unconnected = TRUE, batch_size = 1e5,
                       check_radius = TRUE) {
  bf_msg("Generating between array.\n")

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

  if (is.null(radius)) {
    radius <- mean(res(bf)) / 2
  } else if (check_radius) {
    # Analysis of the effect of changing radius is in test-calc_flux.R

    radius_cells <- radius / mean(res(bf)) # radius converted to cells
    if (radius_cells <= 0.25 || radius_cells >= 1) {
      stop("radius should be less than the resolution and more than 1/4 the ",
           "resolution or the flux is likely to be biased. ",
           "Set check_radius to FALSE to ignore this advice.")
    }
  }

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

    # Determine if each location is between each possible pair of model
    # locations based on whether it's within the `radius` distance (along a
    # great circle) to each of the great circle lines connecting the pairs.
    within_dist <-
      sf::st_is_within_distance(lines_sfc, points_sph,
                               dist = units::set_units(radius, "m"))



    # Update between
    #
    # There seems to be overhead for interacting with the sparse array
    # Making a matrix with three columns which will contain the index
    # of every TRUE cell and then updating between once is much
    # faster than updating between in a loop.


    # Make an index matrix of additional TRUE cells
    # each row is the three dimensional index of a TRUE (between) cell
    add_list <- vector(mode = "list", length = nrow(b_pairs))
    for (i in seq_len(nrow(b_pairs))) {
      n <- length(within_dist[[i]])
      add_list[[i]] <- cbind(rep(b_pairs$from[i], n),
                             rep(b_pairs$to[i], n),
                             within_dist[[i]])
    }

    # Forward transitions
    add <- do.call(rbind, add_list)
    between[add] <- TRUE  # individual cell indexing

    # Backwards transitions
    add <- add[, c(2, 1, 3)]
    between[add] <- TRUE

    bf_msg("    ", round(batch / n_batch * 100, 2), "%\n")

  } # End batch loop

  if (FALSE) {
    # visualize - for coding or debugging
    plot(hull, reset = FALSE)
    plot(points_sph, col = "grey", add = TRUE)
    plot(active_sph, col = "red", add = TRUE)
    i <- 30
    points(a_coords[c(pairs$from[i], pairs$to[i]),  ], col = "blue", cex = 1.2)
    this_line <- lines_sfc[i] |> # nolint
      sf::st_segmentize(units::set_units(10, "km"))
    plot(this_line, add = TRUE, col = "green")
    plot(points_sph[between[pairs$from[i], pairs$to[i], ], ], cex = 2,
         col = "green", add = TRUE)
  }

  return(list(between = between, points = points, radius = radius))
}
