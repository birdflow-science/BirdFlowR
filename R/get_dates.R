#' Retrieve dates component of a BirdFlow model
#'
#' `get_dates()` get date information for a BirdFlow model
#'
#'  \pkg{BirdFlowR} uses the same conventions for assigning dates to
#'  eBird "weeks" as \pkg{ebirdst}. This changed with the 2022 release.
#'
#' \pkg{ebirdst} version <= 2.2021.3
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
#' \pkg{ebirdst} version  >= 3.2022.0
#' * \pkg{ebirdst} dropped `ebirdst_weeks` and  `ebirdst::date_to_st_week()`
#' *  Dates are assigned to weeks by finding which evenly spaced Julian date of
#' a week center Julian date of the input is closest to.
#' * All weeks but the last have 7 days.
#' * On non-leap-years the last week has 8 days, on leap years it has 9.
#'
#' [truncate_birdflow()] creates models with a subset of the intervals
#' and renames them `1:[n_timesteps()]` of the truncated model so if it
#' has been used `week` and `timestep` will not have identical values.
#'
#'
#'
#' @param bf A BirdFlow object to retrieve dates from.
#'
#' @return It returns a data frame.
#'
#' The columns are:
#' \item{timestep}{The model timestep associated with each row. Will always
#' equal the row number.}
#' \item{date}{The date associated with the midpoint of the timestep}
#' \item{label}{ The month and day based label associated with the interval.
#' Consistent with eBird's 2022 date scheme will be one day off the `date`
#' after February on leap years but are still used to label intervals. If the
#' model was fit with an older version of \pkg{ebirdst} they will never be
#' offset}
#' \item{julian}{The Julian date (day of year) associated with the timestep
#' center}
#' \item{week}{The eBirdst week number associated with the date. For full year
#' models this is identical to `timestep` but after
#' [truncation](truncate_birdflow) they will differ.
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
