#' Retrieve dates component of a BirdFlow model
#'
#' `get_dates()` retrieves the dates component of a BirdFlow model.
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
#' has been used the data may differ from `ebirdst::ebirdst_weeks`.
#'
#' @param bf A BirdFlow object to retrieve dates from.
#'
#'
#' @return A data frame with:
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
 return(bf$dates)
}
