# Function to create legend labels with month and day
# from proportion of year  (PY) or half proportion of year (HPY)
# Used by plot_routes()
# PY maps 1 year to 0 to 1
# HPY maps two years to 0 to 1
# In either case this will return formatted labels indicating the
# Month and Day associated with the input values.
format_pyear <- function(x, hpy = TRUE) {

  if(hpy){ # convert hpy to py if necessary
    x <- (x * 2) %% 1  # double and take remainder
  }

  year <- 2023  # arbitrary but shouldn't be a leap year
  dates <- lubridate::as_date(rep("2000-01-01", length(x)))
  lubridate::year(dates) <- year
  lubridate::yday(dates) <- round((x * 366) + 0.5)
  # using ebirdst convention of 366
  # + 0.5 because of 0 vs 1 indexing in posix vs lubridate::yday
  paste(lubridate::month(dates, label = TRUE, abbr = FALSE),
        lubridate::day(dates))
}
