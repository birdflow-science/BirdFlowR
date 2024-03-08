
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
#' The first two dimensions represent pairs of locations within the
#' BirdFlow model (active cells).
#' The third dimension represents the `points`. Each value in the array is
#' `TRUE` if the point is between the associated model cells. Specifically,
#' if the point is within `radius` meters (along a great circle)
#' of the great circle line connecting the cell centers.
#'
#' If `points` and `radius` are `NULL` they default to the cell radius and the
#' center of all cells within the BirdFlow extent that fall between
#' any active cells. This is ussually a superset of the active cells.
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
#'
#' @return A list with:
#' \item{between}{An array with dimensions representing the
#' "from" location, the "to" location, and the `points`. Cells are `TRUE`
#'  if the associated point is between the associated from and to locations.}
#'  \item{points}{A dataframe of points that define the third dimension
#'  in between.  It is identical to the input `points` if they are not `NULL`.
#'  Otherwise it will be a data frame with columns x, y, and i corresponding to
#'  the third dimension in `between`. Note `i` will be `NA` for some points that
#'  aren't within the mask but do fall between active cells.}
#' @keywords internal
is_between <- function(bf,  points = NULL, radius = NULL, n_directions = 1) {

  # Force use of s2
  o_use_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(o_use_s2))
  sf::sf_use_s2(TRUE)

  if(!n_directions == 1) {
    stop("Currently only one direction is supported.")
  }
  # Dimensions
  # 1 from location  n_active()
  # 2 to location    n_active()
  # 3 points  (if NULL use cell centers)
  # 4 (pending) n_direction, directional bins.

  if (is.null(radius)) {
    radius <- mean(res(bf)) / 2
  }

  if (is.null(points)) {

    # Create points at all cell centers in the rectangular raster
    points <- rasterize_distr(get_distr(bf, 1), bf, format = "dataframe")
    points <- points[, c("x", "y", "i")]
    active <- points[!is.na(points$i), , drop = FALSE]

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

  # Initialize array (All FALSE) to hold betweenness
  between <- array(data = FALSE,
                   dim = c(n_active(bf), n_active(bf), nrow(points)),
                   dimnames = list(from = paste0("F_", seq_len(n_active(bf))),
                                   to = paste0("T_", seq_len(n_active(bf))),
                                   loc = paste0("L_", seq_len(nrow(points)))))


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

  # Lines connecting pairs of points
  lines <- vector(mode = "list", length  = nrow(pairs))
  for (i in seq_along(lines)) {
    lines[[i]] <- sf::st_linestring(a_coords[c(pairs$from[i], pairs$to[i]), ])
  }

  # Convert to simple features column and add CRS
  lines_sfc <- sf::st_sfc(lines, crs = "EPSG:4326")

  # Determine if each location is between each possible pair of model locations
  # based on whether it's within the `radius` distance (along a great circle)
  # to each of the great circle lines connecting the pairs.
  within_dist <- sf::st_is_within_distance(lines_sfc, points_sph,
                                           dist = units::set_units(radius, "m"))
  for (i in seq_len(nrow(pairs))) {
    between[pairs$from[i], pairs$to[i], within_dist[[i]]] <- TRUE
    between[pairs$to[i], pairs$from[i], within_dist[[i]]] <- TRUE
  }

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



  return(list(between = between, points = points))
}
