#'
#' Lookup a series of transitions connecting two dates
#'
#' This function returns an ordered vector of transition names that connect
#' start to end.  Currently time is linear and transitions can not cross forward
#' from December into January.  If `start` is later in the year than `end`
#' (regardless of what year) than the transitions will flow backwards in time.
#'
#' @param start,end The starting and ending points in time. In one of the
#'   following formats: character, a date in the form
#'   year-month-day e.g. "2022-11-25" for Nov. 25, 2022); numeric, a
#'   timestep; or  Date, a \code{\link[base::Dates]{Date}} object
#' @param obj A BirdFlow object or the dates component of one.
#' @return A character vector with the named transitions required to get between
#'   `start` and  `end`
#' @export
#'
lookup_transitions <- function(start, end, obj){
  # Currently all based on day of year and can't go forward across end of year
  # (time is a single linear year)
  if("dates" %in% names(obj) & !is.data.frame(obj)){
    obj <- obj$dates
  }

  if(class(start) != class(end))
    stop("start and end must both be specified in the same manner.")
  if(is.character(start)){
    start <- lubridate::as_date(start)
    end <- lubridate::as_date(end)
  }

  if(lubridate::is.Date(start)){
    start_doy <- lubridate::yday(start)+0.5
    end_doy <- lubridate::yday(end)+0.5
    start <- which.min(abs(obj$doy - start_doy))
    end <-  which.min(abs(obj$doy - end_doy))
  }

  stopifnot(is.numeric(start), is.numeric(end),
            length(start) == 1, length(end) == 1)

  if(!start %in% 1:nrow(obj))
    stop("Start resolved to timestep", start, "which isn't a modeled timestep.")
  if(!end %in% 1:nrow(obj))
    stop("Start resolved to timestep", start, "which isn't a modeled timestep.")
  if(start == end)
    stop("Start and stop resolved to same timestep (", start, ")")

  nc <- nchar(nrow(obj))
  pad <- function(x) stringr::str_pad(x, width = nc, pad = "0")
  steps <- seq(start, end, by = ifelse(start < end, 1, -1))

  return( paste0("T_", pad(steps[-length(steps)]), "-", pad(steps[-1]) ) )

}
