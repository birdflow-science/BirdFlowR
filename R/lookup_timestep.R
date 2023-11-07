
#' Lookup timestep
#'
#'  This function returns the timestep or timesteps associated with `x` in a
#'  particular BirdFlow model, where `x` represents dates or timesteps in
#'  various formats.
#'
#'  So far all BirdFlow objects have had timesteps corresponding with weeks of
#'  the year and matching the S&T timesteps. However, it is likely that
#'  we will add the ability to make BirdFlow objects that only model part of the
#'  year. If we do this the timestep values will not necessarily match weeks.
#'  For example a model that covers Week 6, to 20  would have timesteps from 1
#'  to 15.
#'
#'  If `x` is numeric it is assumed to already be a timestep. This is useful
#'  when using this function internally to resolve arguments to other
#'  functions like [route()], [predict()], and [get_distr()].
#'
#' @param x A character object representing date as year-month-day e.g.
#'  "2023-03-29", date object ([`Date`][base::Dates],
#'  [`POSIXct`][base::DateTimeClasses], or [`POSIXlt`][base::DateTimeClasses]),
#'  a numeric timestep, a character representing a timestep e.g. "t1", or "all"
#'  for all timesteps in the model.
#' @param bf A BirdFlow object.
#' @return A vector of timesteps corresponding to elements in `x`.
#' @export
#' @examples
#' bf <- BirdFlowModels::amewoo
#' lookup_timestep(c("2001-3-23", "2022-12-05"), bf)
#'
lookup_timestep <- function(x, bf) {
  stopifnot(inherits(bf, "BirdFlow"))
  dates <- bf$dates
  original_x <- x

  # Special case "all" returns all timesteps in order
  if (length(x) == 1 && is.character(x) &&  tolower(x) == "all")
    return(bf$dates$interval)

  # timesteps e.g. "t1" "t2"
  if (is.character(x) && all(grepl("^t[[:digit:]]*$",
                                   ignore.case = TRUE, x = x))) {
      x <- as.numeric(gsub("t", "", x))
  }

  # convert date like things to dates
  if (is.character(x) || inherits(x, "POSIXt")) {
    x <- lubridate::as_date(x)
  }

  if (lubridate::is.Date(x)) {

    # Calculate the proportion of the year that has passed at each date.
    # using 366 for all years as that's what ebirdst::date_to_st_week does

    # Support old models with a different naming convention
    if (any(grepl("^weeks_", names(dates))))
      names(dates) <- gsub("^weeks_", "", names(dates))

    if (all(c("start", "end") %in% names(bf$dates))) {
      # Note POSIXct$yday starts at 0   lubridate::doy()  starts at 1
      py <- proportion_of_year(x)
      breaks <- c(dates$start[1], dates$end)
      x <- findInterval(py, vec = breaks, all.inside = TRUE)

    } else {   #  SHOULD be DROPPED when we complete switch to dynamic masking
      # Support very old models that don't have columns eith "start" and "end";
      # or "week_start" and "week_end"

      if (nrow(bf$dates) != 52)
        stop("This is unexpected. bf is both lacking some date columns ",
             "AND doesn't include all timesteps. To look up timesteps only",
             " one of those can be true.")
      x <- ebirdst::date_to_st_week(x)

    }
  }  # End is date

  if (!is.numeric(x) || any(is.na(x)) || !all(x %in% dates$interval)) {
    xtext <- ifelse(length(original_x) > 3,
                    paste0(paste(original_x[1:3], collapse = ", "),
                           ", ..."),
                    paste0(original_x, collpase = ", "))
    stop("Date lookup failed for x = ", xtext)
  }
  if (is.integer(x))
    x <- as.numeric(x)
  return(x)
}
