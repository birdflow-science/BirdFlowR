#' Calculate year number from a sequence of timesteps
#'
#' This internal function assigns a sequential year to each
#' observation based on whether the sequence has passed over a presumed year
#' boundary and started over. If the sequence is backwards and passes over the
#' boundary the result will start at 2 (later year) and end at 1 (earlier year).
#'
#' It is used by plot_routes() when calculating half proportional years.
#'
#' @param x a sequence of integers representing timesteps.
#'
#' @return sequence of year numbers all either 1 or 2 depending on whether the
#' corresponding value in x is in the first or second year of x.
#' @keywords internal
calc_year_number <- function(x){
  if(length(x) == 1) # special case
    return(1)

  # Determine direction
  direction <- NULL
  diff <- x[2] - x[1]
  if(diff == 1)
    direction <- "forward"
  if(diff == -1)
    direction <- "backward"
  if(diff > 1)  # diff is large positive if backwards across year boundary
    direction <- "backward"
  if(diff < -1)
    direction <- "forward" #  eg -51 at year boundary
  if(is.null(direction))
    stop("Direction couldn't be resolved from sequence")

  flip <- function(x) x[length(x):1]

  # Logic is different for backwards so instead of handling both cases
  # I'm flipping backwards sequences before and after the calculations
  if(direction == "backward"){
    x = flip(x)
  }

  is_max <- x == max(x)
  year_number <- cumsum(is_max) + 1
  # the above makes the transition one value too soon
  # so move every number back one
  year_number <- c(1, year_number[-length(year_number)])

  if(direction == "backward"){
    year_number <- flip(year_number)
  }

  return(year_number)
}
