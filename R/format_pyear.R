# Function to create legend labels with month and day
# from proportion of year.  Used by plot_routes()
format_pyear <- function(x, format = birdflow_options("time_format")) {
  year <- 2023  # arbitrary but shouldn't be a leap year
  dates <- lubridate::as_date(rep("2000-01-01", length(x)))
  lubridate::year(dates) <- year
  lubridate::yday(dates) <- round((x * 366) + 0.5)
  # using ebirdst convention of 366
  # + 0.5 because of 0 vs 1 indexing in posix vs lubridate::yday
  paste(lubridate::month(dates, label = TRUE, abbr = FALSE),
        lubridate::day(dates))
}
