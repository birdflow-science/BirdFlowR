#' Retrieve dates component of a BirdFlow model
#'
#' `get_dates()` get date information for a BirdFlow model
#'
#' @section Date scheme:
#'
#'  In a non-leap year there are 52 weeks plus 1 day so assigning days
#'  to weeks is a little bit arbitrary.  \pkg{BirdFlowR} uses the same
#'  conventions as \pkg{ebirdst} and eBird itself which changed
#'  with the eBird 2022 version year. Thus for some dates the result
#'  of `lookup_timestep()` will change depending on the `ebird_version_year`
#'  associated with the model.
#'
#'
#' ### 2021 date scheme (\pkg{ebirdst} <= 2.2021.3)
#'
#' * Date table is derived from `ebirdst::ebirdst_weeks` and added
#' to the model object by [preprocess_species()].
#' * Julian date is converted to a proportion with `(jd - 0.5)/366`.
#' * 52 even ranges are assigned to 0 to 1 and the proportional date is then
#'   compared to these thresholds.
#' * In a non-leap year two weeks have 8 days and the last week has 6.
#' * In a leap-year two weeks have 8 days.
#' * All other weeks have 7 days.
#'
#' ### 2022 date scheme (\pkg{ebirdst} >= 3.2022.0)
#' * \pkg{ebirdst} dropped `ebirdst_weeks`
#' *  Weeks are defined by evenly spaced Julian dates (of week center) which
#'    are always `seq(4, 366, 7)`.
#' * Dates are assigned to the week for which the Julian date is closest.
#' * On non-leap-years the last week has 8 days, on leap years it has 9.
#' * All weeks but the last have 7 days.
#' * Week labels (month and day) are assigned based on the month and day
#'   associated with the Julian week center on non-leap years regardless
#'   of whether it is a leap year. The label on leap years will thus be one
#'   day off the date of the week center for most of the year.
#'
#' ### Retrieving dates
#'  The date columns stored within a birdflow model changes depending on the
#'  \pkg{ebirdst} version year, so do NOT use `bf$dates` in your code. Instead
#'  use`get_dates()` which will always return the same (newer) column names.
#'
#' @note
#' * [truncate_birdflow()] creates models with a subset of the intervals
#' and renames them `1:[n_timesteps()]` of the truncated model so if it
#' has been used `week` and `timestep` will not have identical values.
#'
#'
#' @param bf A BirdFlow object to retrieve dates from.
#'
#' @return A data frame with columns:
#' \item{timestep}{The model timestep associated with each row. Will always
#' equal the row number.}
#' \item{date}{The date associated with the midpoint of the timestep}
#' \item{label}{ The month and day based label associated with the timestep
#' Consistent with eBird's 2022 date scheme will be one day off the `date`
#' after February on leap years but are still used to label timesteps. If the
#' model was fit with an older version of \pkg{ebirdst} they will never be
#' offset}
#' \item{julian}{The Julian date (day of year) associated with the timestep
#' center}
#' \item{week}{The \pkg{ebirdst} week number associated with the date. For full
#' year models this is identical to `timestep` but after
#' [truncation](truncate_birdflow) they will differ.}
#'
#' Prior to \pkg{BirdFlowR} v. 0.1.0.9040 it returned columns:
#' \item{interval}{The interval or timestep associated with each date.
#' It will range from 1 to [n_timesteps()].  With full models this is
#' equivalent to the week of the year but with truncated models may
#' not be.}
#'  \item{date}{The date associated with the interval's midpoint.}
#'  \item{midpoint, start, end}{The midpoint, start, and end of each interval
#' as a proportion of the total year that has elapsed.}
#' \item{doy}{The day of year associated with the midpoint of each interval.}
#' @export
get_dates <- function(bf) {

  # Return models fit with ebirdst 3.2022.0 and newer as is
  if (get_metadata(bf, "ebird_version_year") >= 2022)
    return(bf$dates)

  # Reformat older dates (ebirdst 2021)
  dates <- bf$dates
  names(dates)[names(dates) == "interval"] <- "timestep"
  if (!"week" %in% names(dates) && nrow(dates) == 52)
    dates$week <- 1:52
  d <- lubridate::as_date(dates$date)
  dates$label <- paste(
    lubridate::month(d, abbr = FALSE, label = TRUE),
    lubridate::mday(d), sep = " ")
  dates$julian <- lubridate::yday(d)
  dates <- dates[, c("timestep", "date", "label", "julian", "week")]
  return(dates)

}
