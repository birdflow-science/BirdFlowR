#' Generate BirdFlow routes
#'
#' `route()` projects bird positions over time based on the probabilities
#' embedded in a BirdFlow model. The output is linear, stochastic routes.
#'
#' @param bf A BirdFlow object.
#' @param n If sampling starting positions (`x_coord`, and `y_coord` are NULL).
#' Generate this many samples.  Otherwise the `x_coord` and `y_coord` positions
#' will each be duplicated `n` times.
#' @param x_coord,y_coord  Optional, if `NULL` starting points will be drawn
#'  from the species distribution at the initial timestep.
#' @param from_marginals Use `FALSE` (the default) to use
#' distributions derived directly from eBird Status and Trends when sampling
#' starting locations. Set to `TRUE` to
#' sample from distributions derived from the fitted model parameters stored in
#' the marginals. Passed to [get_distr()].
#' @inheritDotParams lookup_timestep_sequence -x
#' @return A BirdFlowRoutes object with columns:
#'    \item{`x`, `y`}{Coordinates of point along route.}
#'    \item{`date`}{Date associated with that point.}
#'    \item{`timestep`}{Timestep associated with point.}
#'    \item{`route`}{Unique ID for that route or individual.}
#'    \item{`i`}{Location index for the point (see [i_to_xy()]).}
#'    \item{`stay_id`}{Within each route a sequential id for locations.}
#'    \item{`stay_len`}{How many timesteps was the Bird at that point during
#'    the stay (minimum of 1).}
#' It also has **experimental** attributes:
#' \describe{
#' \item{`geom`, `species`, `dates`}{The `geom`, `species`, and `dates`
#'   components of the BirdFlow object the routes are derived from.}
#' \item{`metadata`}{The `metadata` component of the parent BirdFlow object,
#'   with one additional item `route_type = "synthetic"`.}
#' }
#' @examples
#' bf <- BirdFlowModels::amewoo
#' rts <- route(bf, 10, season = "prebreeding")
#'
#' \dontrun{
#' plot_routes(rts)
#' }
#'
#' @export
#' @importFrom Matrix Matrix
#' @importMethodsFrom Matrix t
#' @importClassesFrom Matrix Matrix sparseMatrix
#' @importFrom rlang .data
#' @seealso [plot_routes()], [animate_routes()]
route <- function(bf,  n = 1, x_coord = NULL, y_coord = NULL,
                  from_marginals = FALSE, ...) {

  ### BACK COMPATABILITY CODE
  bf <- add_dynamic_mask(bf)  # To ease transition pain

  dyn_mask <- bf$geom$dynamic_mask

  from_coordinates <- !is.null(x_coord) && !is.null(y_coord)

  # Time
  timesteps <-  lookup_timestep_sequence(bf, ...)
  transitions <- as_transitions(timesteps, bf)
  start <- timesteps[1]

  # Convert x and y coordinates input into row and col
  if (from_coordinates) {
    row <- y_to_row(y_coord, bf)
    col <- x_to_col(x_coord, bf)

    # Duplicate starting positions based on n - only if not sampling
    if (!missing(n) && n != 1) {
      if (length(n) == 1) {
        row <- rep(row, each = n)
        col <- rep(col, each = n)
      } else {
        if (length(row) != length(n))
          stop("n should have a single value, or one value for each",
               " position")
        row <- as.vector(unlist(mapply(FUN = rep, x = row,  each = n)))
        col <- as.vector(unlist(mapply(FUN = rep, x = col,  each = n)))
      }
    }

  } else {
    # Full starting coordinates not supplied
    if (!is.null(x_coord) || !is.null(y_coord)) {
        stop("If starting from coordinates use both x_coord and y_coord, ",
             "if not both should be NULL")
    }
    loc <- get_distr(bf, start, from_marginals = from_marginals) |>
      sample_distr(n = n, format = "i", bf = bf)
    row <- i_to_row(loc, bf)
    col <- i_to_col(loc, bf)
  }

  stopifnot(
    `Unequal row and column lengths` = length(row) == length(col),
    `row has values that are not row numbers` = row %in% seq_len(nrow(bf)),
    `col has values that are not column numbers` = col  %in% seq_len(ncol(bf))
  )

  # Create initial distributions (concentrated to single locations)
  # These aren't dynamically masked
  initial_distr <- Matrix::Matrix(0, nrow = n_active(bf), ncol = length(row))
  indices <- rc_to_i(row, col, bf)
  sel <- cbind(indices,  seq_len(length(indices)))
  initial_distr[sel] <- 1

  # Make positions - a list of of state indices that are within the dynamic
  # mask for each timestep
  s <- 1:n_active(bf)
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
    tm <- get_transition(bf,  transitions[i])  # transition matrix
    distr <- tm %*% distr           # project
    distr <- sample_distr(distr)  # "one hot"
    trajectory[i + 1, ] <- extract_positions(distr, timestep = timesteps[i + 1])
  }

  # trajectory to BirdFlowRoutes object
  rts <- format_trajectory(trajectory, bf, timesteps)
  latlon <- xy_to_latlon(rts$x, rts$y, bf)
  rts$lon <- latlon$lon
  rts$lat <- latlon$lat
  rts$timestep <- as.integer(rts$timestep)
  rts$route_type <- 'synthetic'
  rts$date <- as.Date(rts$date)
  rts <- BirdFlowRoutes(rts, species = bf$species, metadata = bf$metadata, geom = bf$geom, dates = get_dates(bf), source='Synthesized from a BirdFlow model')

  return(rts)
}



# Internal helper function to convert one or more trajectories
# stored as a vector or in columns of a matrix into a data.frame
# with x, y, route_id, timestep, date, i, stay_id, and stay_len columns
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
  route_id <- rep(seq_len(ncol(trajectory)), each = nrow(trajectory))
  date <- get_dates(bf)$date[timestep]

  points <- data.frame(x, y, route_id, timestep, date,
                       i = as.vector(trajectory))

  # Adjust dates -- If it crosses the year boundary, add year by 1
  points$date <- as.Date(points$date)
  check_whether_year_plus_one <- function(dates_) {
    for (i in 2:length(dates_)) {
      if (dates_[i] < dates_[i - 1]) {
        dates_[i] <- dates_[i] + lubridate::years(1)
      }
    }
    return(dates_)
  }
  points <- points |>
      dplyr::group_by(.data[['route_id']]) |>
      dplyr::mutate(
        date = check_whether_year_plus_one(.data[['date']])
      ) |>
      dplyr::ungroup() |>
      as.data.frame() |> 
      sort_by_id_and_dates()

  return(points)
}
