#' Calculate year offset from a sequence of timesteps
#'
#' This internal function assigns a year offset for each
#' observation based on whether the sequence has passed over a presumed year
#' boundary and started over. The sequence always starts with 0.
#' Forward sequences increment with each new year, while backwards sequences
#' decrements (0, -1, etc.). Elsewhere there are limitations on `route()` that
#' prevent routes longer than 1 full year (back to start), but this function
#' does not have that limit.
#'
#' It is used by `route()` to convert from circular to linear time by
#' making dates monotonic.
#'
#' @param x a sequence of integers representing timesteps.
#'
#' @return sequence of year offsets these will be 0 or 1 for forward sequences
#' and 0 or -1 for backwards.
#' @keywords internal
calc_year_offset <- function(x) {
  if (length(x) == 1) # special case
    return(1)

  # Determine direction
  direction <- NULL
  diff <- x[2] - x[1]
  if (diff == 1)
    direction <- "forward"
  if (diff == -1)
    direction <- "backward"
  if (diff > 1)  # diff is large positive if backwards across year boundary
    direction <- "backward"
  if (diff < -1)
    direction <- "forward" #  eg -51 at year boundary
  if (is.null(direction))
    stop("Direction couldn't be resolved from sequence")

  # Logic is different for backwards so instead of handling both cases
  # I'm flipping backwards sequences before and after the calculations
  if (direction == "backward") {
    x <- base::rev(x)
  }

  calculate_year_offset <- function(timesteps) {
    c(0, cumsum(timesteps[-1] < timesteps[-length(timesteps)]))
  }

  year_offset <- calculate_year_offset(x)

  if (direction == "backward") {
    year_offset <- rev(year_offset)
    year_offset <- year_offset - max(year_offset)
  }

  return(year_offset)
}
