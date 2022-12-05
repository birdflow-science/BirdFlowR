#' Generate BirdFlow routes
#'
#' route() projects bird positions over time based on the probabilities
#' embedded in a BirdFlow model. The output is stochastic.
#'
#' @param x A BirdFlow model object
#' @param x_coord,y_coord  One or more sets of coordinates identifying starting
#' positions.
#' @param n Optional, if provided each starting position will be duplicated this
#' many times. `n` can be a single integer or a vector with one integer per starting
#' position.
#' @param row,col One or more row and column indices to begin routing from. These
#' are an alternative to `x_coord` and `y_coord` and both sets of parameters
#' should not be used at the same time.
#' @param start,end These define the time period to route over. They can be
#' [Date][base::date] objects, integer timesteps, or a string with
#' "year-month-day" e.g. "2022-11-28".
#' @return This will likely change. Currently returns a list with:
#' \item{points}{A data.frame with coordinates, date, and route id}
#' \item{lines}{a [sf][sf::sf] object containing one line per route.}
#' @export
#' @importFrom Matrix Matrix
#' @importMethodsFrom Matrix t
#' @importClassesFrom Matrix Matrix sparseMatrix
#' @importFrom magrittr %>%
#' @examples
route <- function(x, x_coord, y_coord, n, row, col, start, end){

  # Convert x and y coordinates input into row and col
  if(!missing(x_coord)){
    if(missing(y_coord))
       stop("If using x_coord you must also use y_coord.")
    if(!missing(row) || !missing(col))
      stop("If using x_coord and y_coord don't also use row or col.")
    row <- y_to_row(y_coord, x)
    col <- x_to_col(x_coord, x)
  }

  stopifnot(
    `Unequal row and column lengths` = length(row) == length(col),
    `row has values that are not row numbers` = row %in% 1:x$geom$nrow,
    `col has values that are not column numbers` = col  %in% 1:x$geom$ncol
    )

  # Duplicate starting positions based on n
  if(!missing(n)){
    if(length(n) == 1){
      row <- rep(row, each = n)
      col <- rep(col, each = n)
    } else {
      if(length(row) != length(n))
        stop("n should have a single value, or one value for each position")
      row <- as.vector(unlist(mapply(FUN = rep, x = row,  each = n)))
      col <- as.vector(unlist(mapply(FUN = rep, x = col,  each = n)))
    }
  }

  transitions <- lookup_transitions(start, end, x)

  # Create initial state with one 1 per column
  # each column represents a single model state
  # There is a column for each initial position - for each value in row and col.
  initial_state <- Matrix::Matrix(0, nrow = x$n, ncol = length(row))
  indices <- rc_to_i(row, col, x)
  sel <- cbind(indices, 1:length(indices)) # 2 column matrix of start positions
  initial_state[sel] <- 1

  extract_positions <- function(x){
    apply(x,MARGIN = 1,  function(x) which(as.logical(x)))
  }

  state <- t(initial_state) # for multiplication need to make columns = state
  # could get around this by storing a transposed
  # tm on import and changing order on matrix multiplication

  # Trajectory will hold the route information in a matrix with
  #  dimensions: timesteps, routes
  #  values: the index of the state in the state matrix
  trajectory <- matrix(nrow = length(transitions) + 1, ncol = length(row))
  dimnames(trajectory) <-  list(timestep = NULL, route = NULL)
  trajectory[1, ] <- extract_positions(state)

  # Projection
  for(i in seq_along(transitions)){
    tm <- x$trans[[transitions[i]]] # transition matrix
    state <- state %*% tm           # project
    state <- t(sample_state(t(state)))  # probabalistic new state
    trajectory[i+1, ] <- extract_positions(state) # save the state
  }

  format_trajectory <- function(trajectory, obj){
    # dimensions of trajectory are timestep and route
    # values are the state space index of the location at the time and route
    # Converting to a long format. With columns:
    #  x, y : coordinates of position
    #  timestep : integer timestep, corresponds to rows in the dates element of x
    #  route : integer route ID
    #  date : the date associated with the timestep
    x <- as.vector(i_to_x(trajectory, obj))
    y <- as.vector(i_to_y(trajectory, obj))
    timestep <- rep(1:nrow(trajectory), times = ncol(trajectory))
    route <- rep(1:ncol(trajectory), each = nrow(trajectory))
    date <- obj$dates$date[timestep]
    return( data.frame(x, y, route, timestep, date) )
  }

  # Makes x and y vectors into lines
  convert_to_lines <- function(x, y)
    sf::st_linestring(cbind(x, y), "XY")

  # Create an sf object with lines for each route
  convert_route_to_sf <- function(x){
    x %>% dplyr::group_by(route) %>%
      dplyr::summarize(geometry = sf::st_geometry(convert_to_lines(x, y)) ) %>%
      as.data.frame() %>% sf::st_as_sf()
  }

  points <- format_trajectory(trajectory, x)
  lines <- convert_route_to_sf(points)

  return(list(points = points, lines = lines))
}
