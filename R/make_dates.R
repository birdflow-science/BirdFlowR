

#' Internal function to make the dates component of a BirdFlow model
#'
#' Called from [preprocess_species] and not intended for other use.
#' @param version_year leave NULL for typical usage.  Set to
#' a eBird version year to override.  Used by `switch_date_format()`.
#' version_year < 2021 will yield old date format.
#' @return Dates table appropriate for the current version of \pkg{ebirdst}
#' @keywords internal
make_dates <- function(version_year = NULL) {

  # With 2023 data release potentiall date format
  if (is.null(version_year))
    version_year <- ebirdst::ebirdst_version()$version_year

  if (version_year < 2022) {
    # Reformat and export dates
    dates <- as.data.frame(ebirdst_weeks)
    # Note ebirdst_weeks now stored in BirdFlowR is a copy of the ebirdst
    # version that was dropped with the 2022 data release (nov 2023)
    names(dates)[names(dates) == "week_number"] <- "interval"
    dates$doy <- lubridate::yday(dates$date) + 0.5
    dates$date <- as.character(dates$date)

    # Rename ("week_" columns by dropping preffix )
    names(dates) <- gsub("^week_", "", names(dates))

    # Duplicate interval column as week so that week number is preserved
    # in truncated models
    dates$week <- dates$interval

    return(dates)
  }

  # ebirdst v 3.2022 and later
  dates <- data.frame(timestep = 1:52,
                      date = NA,
                      label = NA,
                      julian = seq(4, 366, 7),
                      week = 1:52)

  # Labels (don't change on leap years - per ebirdst 2022+)
  ts_dates <- rep(lubridate::today(), nrow(dates))
  lubridate::year(ts_dates) <- 1999 # FIXED! - any non-leap year works
  lubridate::yday(ts_dates) <- dates$julian

  dates$label <- paste(
    lubridate::month(ts_dates, abbr = FALSE, label = TRUE),
    lubridate::mday(ts_dates))

  # Dates - actual date associated with center of week
  # It Shifts one day relative to labels for weeks after Feb. on leap years.
  lubridate::year(ts_dates) <- version_year
  lubridate::yday(ts_dates) <- dates$julian
  dates$date <- as.character(ts_dates)

  return(dates)
}
