
#' lookup timestep
#'
#'  This function returns the timestep or timesteps associated with `x` in a
#'  particular BirdFlow model, where `x` dates or timesteps in various formats.
#'
#'  So far all BirdFlow objects have had timesteps corresponding to weeks of
#'  the year and matching the S&T timesteps. However, it is likely that
#'  we will add the ability to make BirdFlow objects that only model part of the
#'  year. If we do this the timestep values will not necessarily match weeks.
#'
#'  If `x` is numeric it is assumed to already be a timestep. This is useful
#'  when using this function internally within [route()] and [predict()].
#'
#' @param x a character object representing date as year-month-day e.g.
#'  "2023-03-29", date object, or a numeric timestep.
#' @param bf a BirdFlow object
#' @return a vector of timesteps corresponding to elements in `x`
#' @export
#' @examples
#' bf <- BirdFlowModels::amewoo
#' lookup_timestep(c("2001-3-23", "2022-12-05"), bf)
#'
lookup_timestep <- function(x, bf){
  stopifnot(inherits(bf, "BirdFlow"))
  dates <- bf$dates
  original_x <- x


  if(is.character(x) || inherits(x, "POSIXt")){
    x <- lubridate::as_date(x)
  }

  if(lubridate::is.Date(x)) {

    # Calculate the proportion of the year that has passed at each date.
    # using 366 for all years as that's what ebirdst::date_to_st_week does

    dates <- bf$dates

    # Support old models with a different naming convention
    if(any(grepl("^weeks_", names(dates))))
      names(dates) <- gsub("^weeks_", "", names(dates))

    if(all(c("start", "end") %in% names(bf$dates))){
      # New code: should work with models pre-processed with >= 0.0.0.9073
      # BirdFlowModels have been converted so should use this code as well.

      # I'm copying methodology from ebirdst::date_to_st_week()

      # Note POSIXct$yday starts at 0   lubridate::doy()  starts at 1
      py <- (as.POSIXlt(x)$yday + 0.5) / 366  # proportion of year I think 366 is a bug
                                      # as POSIXlt(x)yday maxes out at 365
                                      # (on a leap year)

      breaks <- c(dates$start[1], dates$end)
      x <- findInterval(py, vec = breaks, all.inside = TRUE)

    }  else {
      # Support legacy models that don't have columns "start" and "end", or
      # "week_start" and "week_end"
      #  SHOULD be DROPPED when we switch to  dynamic masking
      #   as those will inherently be new models

      if (nrow(bf$dates) != 52)
        stop("This is unexpected. bf is both lacking some date columns ",
             "AND doesn't include all timesteps. To look up timesteps only",
             " one of those can be true.")
      x <- ebirdst::date_to_st_week(x)

    }
  }  # End is date


  if(!is.numeric(x) || any(is.na(x)) || !all(x %in% dates$interval)){
    xtext <- ifelse(length(original_x) > 3,
                    paste0(paste(original_x[1:3], collapse = ", "),
                           ", ..."),
                    paste0(original_x, collpase = ", "))
    stop("Date lookup failed for x = ", xtext)
  }
  if(is.integer(x)) x <- as.numeric(x)
  return(x)
}
