
#' Calculate the average movement for cells in a BirdFlow model
#'
#' `calc_movement_vectors()` calculates the average modeled movement of birds
#' from each location over a single transition in a BirdFlow model.
#'
#' In practice each row of the transition matrix represents a single starting
#' location (`start_x` and `start_y`) and the values in the row represent
#' transition probability to a number of destination cells. The probabilities
#' are used as weights to calculate an average of both the destination x and y
#' coordinates (`end_x`, and `end_y`).  The ending coordinates of the arrow
#' represented the expected average destination of all birds that start
#' in the cell.
#'
#' @param bf A BirdFlow model
#' @param start The starting timestep for the transition to be modeled
#' @param direction "Forward" by default. Set to "backward" to calculate
#' vectors for a transition backwards in time.
#' @return A data frame with columns:
#' \item{i}{The location index of the starting location.  This is the row in
#' the distribution matrix that corresponds to the location.}
#' \item{start}{The starting timestep of the transition.}
#' \item{end}{The ending timestep of the transition.}
#' \item{start_x}{The x coordinate of the starting cell of the transition.}
#' \item{start_y}{The y coordinate of the starting cell of the transition.}
#' \item{end_x}{The weighted average destination x coordinate of all transitions
#'  from the starting cell, with weights set to the transition probability for
#'   each destination.}
#' \item{end_y}{The weighted average destination y coordinate. }
#' \item{weight}{This is the proportion of the population at the starting cell
#' in the eBird S&T distribution for the starting timestep.}
#' \item{width}{This is a range rescaling of weight and is used by both
#' [plot_movement_vectors()], and [animate_movement_vectors()] to set the line
#' width of the arrows.}
#' @export
#' @seealso [plot_movement_vectors()] and [animate_movement_vectors()] call
#' this function and visualize the results.
#' @examples
#' bf <- BirdFlowModels::amewoo
#' mv <- calc_movement_vectors(bf, 7)
#'
calc_movement_vectors <- function(bf, start, direction = "forward") {

  direction <- tolower(direction)
  stopifnot(direction %in% c("forward", "backward"))

  ### Back compatibility code - delete after BirdFlowModels is updated
  if (!has_dynamic_mask(bf))
    bf <- add_dynamic_mask(bf)

  start <- lookup_timestep(start, bf)
  if (direction == "forward") {
    end <- ifelse(start == n_timesteps(bf), 1, start + 1)
  } else {
    end <- ifelse(start == 1, n_timesteps(bf), start - 1)
  }
  end <- lookup_timestep(end, bf)

  trans_name <- lookup_transitions(bf, start = start, end = end,
                                   direction = direction)
  stopifnot(length(trans_name) == 1)

  trans <- get_transition(bf, trans_name)
  start_dm <- get_dynamic_mask(bf, start)
  start_loc <- which(start_dm)
  start_x  <- i_to_x(start_loc, bf)  # x location of each starting position
  start_y  <- i_to_y(start_loc, bf)
  end_dm <- get_dynamic_mask(bf, end)
  end_loc <- which(end_dm)

  # x and y locations associated with each ending position
  end_x <- i_to_x(end_loc, bf)
  end_y <- i_to_y(end_loc, bf)

  # Calculate weights from starting distributions
  start_distr <- get_distr(bf, start)
  distr_weights <- start_distr[start_dm]

  # Calculate the (mean) ending location (pointy end of arrow in plots)
  mean_end_x <- apply(trans * end_x, 2, sum)
  mean_end_y <- apply(trans * end_y, 2, sum)

  d <- data.frame(i = start_loc,
                  start = start,
                  end = end,
                  start_x,
                  start_y,
                  end_x = mean_end_x,
                  end_y = mean_end_y,
                  weight = distr_weights)


  no_transition <- apply(trans, 2, sum) == 0
  d <- d[!no_transition, , drop = FALSE]

  d$width <- range_rescale(d$weight, .085, .7)
  return(d)
}
