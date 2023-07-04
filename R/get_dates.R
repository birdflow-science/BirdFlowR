#' Retrieve dates component of a BirdFlow model
#'
#' `get_dates()` retrieves the dates component of a BirdFlow model.  Most of
#' the resulting data is derived from [ebirdst::ebirdst_weeks] and added
#' to the model object by   [preprocess_species()].  [truncate_birdflow()]
#' creates models with a subset of the intervals and renames them
#' `1:[n_timesteps()]` of the truncated model so if it has been used the
#' data may differ from [ebirdst::ebirdst_weeks()].
#'
#' @param bf a BirdFlow object to retrieve dates from.
#'
#'
#' @return a data frame with
#' \item{interval}{The interval or timestep associated with each date.
#' It will range from 1 to [n_timesteps()].  With full models this is
#' equivalent to the week of the year but with truncated models may
#' not be.}
#'  \item{date}{The date associated with the interval's midpoint}
#'  \item{midpoint, star, end}{The midpoint, start, and end of each interval
#' as a proportion of the total year that has elapsed.}
#' \item{doy}{The day of year associated with the midpoint of each interval.}
#' @export
get_dates <- function(bf) {
 return(bf$dates)
}
