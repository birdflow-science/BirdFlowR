#'
#' Lookup a series of transitions connecting two dates or timesteps
#'
#' `lookup_transitions()` returns an ordered vector of transition names that
#' connect start to end. If `start` and `end` are dates than their order
#' determines whether the transitions flow forward or backward in time.  If they
#' are timesteps than the `direction` argument should be used to indicate
#' whether to project "forward" or "backward" in time possibly passing the year
#' boundary.
#'
#' @details Transitions are named "T_\[from\]-\[to\]" where \[from\] and \[to\]
#'   are timesteps padded with zeros. Direction is important; "T_03-04"
#'   represents a transition backward in time.
#' @param x A BirdFlow object
#' @param start,end The starting and ending points in time. In one of the
#'   following formats: character, a date in the form year-month-day e.g.
#'   "2022-11-25" for Nov. 25, 2022); numeric, a timestep; or  Date, a
#'   [`Date`][base::Dates] object.
#'
#' @param direction Either "forward" or "backward". Only used if `start` and
#'   `end` are timesteps (numeric). Otherwise the direction will be determined
#'   by the dates - forward if `start` and `end` are in chronological order, and
#'   backward if the `end` date is before the `start` date. If `start` and `end`
#'   are timesteps and `direction` is omitted than the direction will default to
#'   "forward" regardless of which timestep is larger - possibly passing over
#'   the year boundary from the last timestep to the first.
#'
#' @return A character vector with the named transitions required to get between
#'   `start` and  `end`
lookup_transitions <- function(x, start, end, direction){

  stopifnot(inherits(x, "BirdFlow"))
  dates <- x$dates

  if(is.integer(start)) start <- as.numeric(start)
  if(is.integer(end)) end <- as.numeric(end)


  if(class(start) != class(end))
    stop("start and end must both be specified in the same manner.")
  if(is.character(start)){
    start <- lubridate::as_date(start)
    end <- lubridate::as_date(end)
  }

  if(lubridate::is.Date(start)){
    start_doy <- lubridate::yday(start)+0.5
    end_doy <- lubridate::yday(end)+0.5
    start <- which.min(abs(dates$doy - start_doy))
    end <-  which.min(abs(dates$doy - end_doy))
    direction <- ifelse(end > start, "forward", "backward")
  }

  stopifnot(is.numeric(start), is.numeric(end),
            length(start) == 1, length(end) == 1)

  if(missing(direction)) direction <- "forward"
  stopifnot(direction %in% c("forward", "backward"))
  if(!start %in% 1:nrow(dates))
    stop("Start resolved to timestep", start, "which isn't a modeled timestep.")
  if(!end %in% 1:nrow(dates))
    stop("Start resolved to timestep", start, "which isn't a modeled timestep.")
  if(start == end)
    stop("Start and stop resolved to same timestep (", start, ")")

  nc <- nchar(nrow(dates))
  pad <- function(x) stringr::str_pad(x, width = nc, pad = "0")


  # loops is a flag that indicates we pass over the year boundary from last
  # timestep to first or from first to last
  is_backward <- direction == "backward"
  step <- ifelse(is_backward, -1, 1)
  loops <- start == end | ( start < end & is_backward) |
    start > end & !is_backward
  if(loops){
    last_ts <- nrow(dates)
    edge1 <- ifelse(is_backward, 1, last_ts)
    edge2 <- ifelse(is_backward, last_ts, 1)
    steps <- c(seq(start, edge1, step), seq(edge2, end, step))
  } else {
    steps <- seq(start, end, by = step)
  }
  return( paste0("T_", pad(steps[-length(steps)]), "-", pad(steps[-1]) ) )

}
