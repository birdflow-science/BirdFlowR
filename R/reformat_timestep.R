
#' reformat timestep labels
#'
#' given a vector of timestep labels provide a vector of formatted labels based
#' on the value of [birdflow_options("time_format")][birdflow_options()] (see
#' that function for options.)
#'
#' Internally distributions are labeled with "t" and the timestep integer.
#' When returning them to the user [reformat_distr_labels()] is called to change
#' the format which in turn calls this function.
#'
#' @param x  one (vector) or more (matrix) distributions, with column labels
#'   consisting of a "t" and the timestep.
#' @param bf A BirdFlow object
#' @keywords internal
#' @return x with update column labels, as dictated by
#'   [birdflow_options("time_format")][birdflow_options()]
reformat_timestep <- function(x, bf) {
  # Given a character vector of timestep labels eg c("t1', "t2")
  # return a vector indicating time in the format specified
  # by birdflow_options("time_format")
  format <- birdflow_options("time_format")

  if (format == "timestep")
    return(x)

  timestep <- as.numeric(gsub("^t", "", x))
  if (anyNA(timestep))
    stop("Unrecognized timestep labels")


  d <- get_dates(bf) # data frame with standard date info for bf
  mv <- match(timestep, d$timestep) # match vector for aligning to timesteps
  dates <- lubridate::as_date(d$date[mv]) # dates associated with timesteps


  if (format == "month_day") {
    return(d$label[mv])
  }

  if (format == "date") {
    return(as.character(dates))
  }

  if (format == "week") {
    return(paste0("w", stringr::str_pad(d$week[mv], width = 2, pad = "0")))
  }

  stop("Unrecognized date format. ",
       "This is probably a programming error in the BirdFlowR package.")

}
