#' Internal function to determine timestep sequence direction and whether
#'it crosses year boundary
#'
#' It assumes that all lags between adjacent steps that don't cross the year
#'  boundary are either -1 or 1.
#'
#' @param ts A sequence of timesteps
#' @return A list with:
#' \item{direction}{Either "forward" or "backward"}
#' \item{loops}{TRUE if the sequence crosses the year boudary, FALSE if it
#' does not.}
#'
#' @keywords internal
ts_info  <- function(ts){
  if(length(ts) == 1)
    return(list(loops = FALSE, direction = NA))
  diffs <- ts[-1] - ts[-(length(ts))]

  # loops is TRUE if sequence crosses the year boundary
  loops <- !all(diffs %in% c(1, -1))

  # Determine direction
  direction <- NULL
  diff <- diffs[1]
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

  return(list(loops = loops, direction = direction))
}
