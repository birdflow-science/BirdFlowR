#' Generate BirdFlow routes
#'
#' `route()` projects bird positions over time based on the probabilities
#' embedded in a BirdFlow model. The output is linear, stochastic routes.
#'
#' @param x A BirdFlow object
#' @param x_coord,y_coord  One or more sets of coordinates identifying starting
#' positions.
#' @param n_each Optional, if provided each starting position will be
#' duplicated this many times. `n_each` can be a single integer or a vector
#' with one integer per starting position.
#' @param row,col One or more row and column indices to begin routing from.
#' These are an alternative to `x_coord` and `y_coord` and both sets of
#' parameters should not be used at the same time.
#' @inheritParams lookup_timestep_sequence
#' @return This will likely change. Currently returns a list with:
#' \item{points}{A dataframe with columns:
#' \describe{
#'    \item{`x`, `y`}{coordinates of point along route}
#'    \item{`date`}{date associated with that point}
#'    \item{`timestep`}{timestep associated with point}
#'    \item{`route`}{unique id for that route or individual}
#'    \item{`i`}{location index for the point (see [i_to_xy()])}
#'    \item{`stay_id`}{within each route a sequential id for locations}
#'    \item{`stay_len`}{how many timesteps was the Bird at that point during
#'    the stay (minumum of 1)}
#'    }
#'  }
#' \item{lines}{a [sf][sf::sf] object containing one line per route.}
#' @export
#' @importFrom Matrix Matrix
#' @importMethodsFrom Matrix t
#' @importClassesFrom Matrix Matrix sparseMatrix
#' @importFrom rlang .data
#' @seealso [route_migration()]
route <- function(x, x_coord, y_coord, n_each,
                  row, col, start, end, direction,
                  season_buffer, n) {

  ### BACK COMPATABILITY CODE
  x <- add_dynamic_mask(x)  # To ease transition pain


  dyn_mask <- x$geom$dynamic_mask

  # Convert x and y coordinates input into row and col
  if (!missing(x_coord)) {
    if (missing(y_coord))
       stop("If using x_coord you must also use y_coord.")
    if (!missing(row) || !missing(col))
      stop("If using x_coord and y_coord don't also use row or col.")
    row <- y_to_row(y_coord, x)
    col <- x_to_col(x_coord, x)
  }

  stopifnot(
    `Unequal row and column lengths` = length(row) == length(col),
    `row has values that are not row numbers` = row %in% seq_len(nrow(x)),
    `col has values that are not column numbers` = col  %in% seq_len(ncol(x))
    )

  # Duplicate starting positions based on n_each
  if (!missing(n_each)) {
    if (length(n_each) == 1) {
      row <- rep(row, each = n_each)
      col <- rep(col, each = n_each)
    } else {
      if (length(row) != length(n_each))
        stop("n_each should have a single value, or one value for each",
             " position")
      row <- as.vector(unlist(mapply(FUN = rep, x = row,  each = n_each)))
      col <- as.vector(unlist(mapply(FUN = rep, x = col,  each = n_each)))
    }
  }

  # This is a sequence of transition codes to progress through
  transitions <- lookup_transitions(x, start, end, direction, season_buffer, n)
  timesteps <- as.numeric(c(gsub("^T_|-[[:digit:]]+$", "", transitions[1]),
                            gsub("^.*-", "", transitions)))

  # Re-define start and end as timesteps (if they aren't already)
  start <- timesteps[1]
  end <-  timesteps[length(timesteps)]
  stopifnot(is.numeric(start), is.numeric(end),
            length(start) == 1, length(end) == 1,
            c(start, end) %in% x$dates$interval)

  # Create initial distributions (concentrated to single locations)
  # These aren't dynamically masked
  initial_distr <- Matrix::Matrix(0, nrow = n_active(x), ncol = length(row))
  indices <- rc_to_i(row, col, x)
  sel <- cbind(indices,  seq_len(length(indices)))
  initial_distr[sel] <- 1

  # Make positions - a list of of state indices that are within the dynamic
  # mask for each timestep
  s <- 1:n_active(x)
  positions <- apply(dyn_mask, 2, function(x) s[x])
  extract_positions <- function(x, timestep) {
    # given a dynamically masked distribution generate state space index i

    pos <- positions[[timestep]]  # positions associated with the d. masked
                                  # distribution for this timestep
    if (is.null(dim(x)))
      return(pos[as.logical(x)])

    apply(x, 2, function(vals)  pos[as.logical(vals)])
  }

  # Add dynamic mask
  distr <- initial_distr[dyn_mask[, start], ]


  # Trajectory will hold the route information in a matrix with
  #  dimensions: time steps, routes
  #  values: the index of the location in the distribution matrix
  trajectory <- matrix(nrow = length(transitions) + 1, ncol = length(row))
  dimnames(trajectory) <-  list(timestep = NULL, route = NULL)
  trajectory[1, ] <- extract_positions(distr, timestep = start)

  distr <- Matrix::Matrix(distr, sparse = TRUE)
  for (i in seq_along(transitions)) {
    tm <- get_transition(x,  transitions[i])  # transition matrix
    distr <- tm %*% distr           # project
    distr <- sample_distr(distr)  # "one hot"
    trajectory[i + 1, ] <- extract_positions(distr, timestep = timesteps[i + 1])
  }

  points <- format_trajectory(trajectory, x, timesteps)
  lines <- convert_route_to_sf(points)
  sf::st_crs(lines) <- sf::st_crs(crs(x))
  return(list(points = points, lines = lines))
}


# Internal helper functions

# Make x and y vectors into lines
convert_to_lines <- function(x, y) {
  sf::st_linestring(cbind(x, y), "XY")
}

# Create an sf object with lines for each route
convert_route_to_sf <- function(x) {
  x |>
    dplyr::group_by(route) |>
    dplyr::summarize(
      geometry = sf::st_geometry(convert_to_lines(.data$x, .data$y))) |>
    as.data.frame() |>
    sf::st_as_sf()
}


# Internal helper function to convert one or more trajectories
# stored as a vector or in columns of a matrix into a data.frame
# with x, y, route, timestep, date, i, stay_id, and stay_len columns
format_trajectory <- function(trajectory, bf, timesteps) {
  # dimensions of trajectory are timestep and route
  # values are the index i of the location at the time and route
  # Converting to a long format. With columns:
  #  x, y : coordinates of position
  #  timestep : integer timestep, corresponds to rows in the dates element of x
  #  route : integer route ID
  #  date : the date associated with the timestep
  x <- as.vector(i_to_x(trajectory, bf))
  y <- as.vector(i_to_y(trajectory, bf))
  timestep <- rep(timesteps, times = ncol(trajectory))
  route <- rep(seq_len(ncol(trajectory)), each = nrow(trajectory))
  date <- bf$dates$date[timestep]

  points <- data.frame(x, y, route, timestep, date, i = as.vector(trajectory))

  add_stay_id <- function(df) {
    # Benjamin's function
    df |>
      dplyr::mutate(stay_id = cumsum(c(1, as.numeric(diff(.data$i)) != 0)),
                    stay_len = rep(rle(.data$stay_id)$lengths,
                                   times = rle(.data$stay_id)$lengths))
  }

  points <- points |> dplyr::group_by(.data$route) |> add_stay_id()

  return(as.data.frame(points))
}
