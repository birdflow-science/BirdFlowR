
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
#' @param allow_failure If TRUE function will return NA values when it fails
#' to resolve a timestep for any element of `x`. With the default, FALSE,
#' the function will throw an error if not all elements of `x` are resolved
#' to timesteps.
#' @return A vector of timesteps corresponding to elements in `x`.
#' @export
#' @examples
#' bf <- BirdFlowModels::amewoo
#' lookup_timestep(c("2001-3-23", "2022-12-05"), bf)
#'
lookup_timestep <- function(x, bf, allow_failure = FALSE) {
  stopifnot(inherits(bf, "BirdFlow"))
  dates <- get_dates(bf) # standardize to current date format
  original_x <- x

  # Special case "all" returns all timesteps in order
  if (length(x) == 1 && is.character(x) &&  tolower(x) == "all")
    return(dates$timestep)

  # timesteps e.g. "t1" "t2"
  if (is.character(x) && all(grepl("^t[[:digit:]]*$",
                                   ignore.case = TRUE, x = x))) {
    x <- as.numeric(gsub("t", "", x, ignore.case = TRUE))
  }

  # convert date like things to dates
  if (is.character(x) || inherits(x, "POSIXt")) {
    x <- lubridate::as_date(x)
  }

  # Process dates
  if (lubridate::is.Date(x)) {
    week <- date_to_week(x, get_metadata(bf, "ebird_version_year"))
    x <- dates$timestep[match(week, dates$week)]
  }

  # Eliminate out of range values
  x[!x %in% dates$timestep] <- NA

  # Check that x is numeric (should always be TRUE by now)
  if(!is.numeric(x))
    stop("Date lookup failed. Likely the class of x was unexpected.")

  # Force integers to numeric
  x <- as.numeric(x)

  # Validate
  if ( any(is.na(x)) && !allow_failure) {
    xtext <- ifelse(length(original_x) > 3,
                    paste0(paste(original_x[1:3], collapse = ", "),
                           ", ..."),
                    paste0(original_x, collapse = ", "))
    stop("Date lookup failed for x = ", xtext)
  }
  return(x)
}
