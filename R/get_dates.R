# Time functions
# Copied from Shiny App as is



#' Function to calculate the midpoint in each interval (probaly week) in the
#' data.
#'
#' Derived from st_week_dates() in the BirdFlowForescast shiny application. This
#' is generalized to work with various interval lengths, but for now intervals
#' are weeks and n = 52.
#'
#' By necessity one week ends up longer - 52 weeks = 264 days.
#'
#' This function returns a [base::data.frame] that describes
#' the midpoint of each interval.
#'
#' @param year the year for which dates are calculated
#' @param n the year is broken into this many even intervals.
#'
#' @return data frame with
#' \item{doy}{The day of year associated with the midpoint of each interval.}
#' \item{interval}{The interval ID; it will range from 1 to n.}
#' \item{date}{The date associated with the interval's midpoint}
#' }
#'
#' @examples
#' get_dates(2019)
get_dates <- function(year, n = 52) {

  # proportional end points of each interval
  srd_date_vec <- seq(from = 0, to = 1, length.out = n + 1)
  # Convert to midpoints
  srd_date_vec <- (srd_date_vec[1:n] + srd_date_vec[2:(n +  1)])/2  #                                                     1)])/2
  srd_date_vec <- round(srd_date_vec, digits = 4)

  days_in_year = 366  # this was from the original function
                      # but wasn't decoared as a constant.
                      # I'm not sure why it wasn't 365.25
                      # with either value there's one 8 day week in
                      # the year.

  days_in_year = 365 + ifelse(leap_year(year), 1, 0)

  p_time <- strptime(x = paste(round(srd_date_vec * days_in_year), year), "%j %Y")

  dates <- ymd(paste(paste0(year),
                          formatC(p_time$mon + 1, width = 2, format = "d", flag = "0"),
                          formatC(p_time$mday, width = 2, format = "d", flag = "0"),
                          sep = "."))

  result <- data.frame(doy=yday(dates)+0.5, # add half a day = noon
                    interval=1:length(dates),
                    date=dates)


  return(result)
}
