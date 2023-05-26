#' Generate breakpoints in proportion of year values that correspond to nice
#' dates
#'
#' This internal function is used by [plot_routes()] to determine where to
#' set the breaks (labels) in the color scale. The color corresponds to dates
#'  represented as a proportion of the year.  This functions picks breaks that
#'  map to quarters of the year, the first of the month, the first and fifteenth
#'  of the month, or ebirds S&T nominal weeks. Choosing from the above based on
#'  which method is closest to the target number of breaks (`target_n`).
#'
#' @param range A range in proportion of year (PY) or half proportion of year
#'  (HPY) values over which breaks will be calculated.
#' @param bf A BirdFlow object (just used for $dates component)
#' @param target_n The target number of breaks
#' @param as_hpy if TRUE than treat range as HPY. If FALSE as PY.
#'
#' @return a sequence of break points in pyear units.
#' @keywords internal
make_pyear_breaks <- function(range, bf, target_n = 8, hpy = TRUE){
  stopifnot(is.numeric(range), !is.na(range))

  # Super small helper functions

  # Return subset that are in range
  select_in_range <- function(values, range){
    values[values >= range[1] & values <= range[2]]
  }

  # Convert py breakpoints to hpy breakpoints
  # this roughly doubles the number of (potential) breakpoints
  # as hpy represents two years so each breakpoint occurs twice
  py_to_hpy<- function(pv){
    sort(unique(c( pv * 0.5, pv * 0.5 + 0.5)))
  }
  #----------------------------------------------------------------------------#
  # Define proportion of year breakpoints under various schemes
  #----------------------------------------------------------------------------#


  quarters <- proportion_of_year(c("2023-12-31",
                                   "2023-10-01",
                                   "2023-07-01",
                                   "2023-04-01",
                                   "2023-01-01"))

  firsts <- proportion_of_year(paste0("2023-", 12:1, "-01"))

  fifteenths <- proportion_of_year(paste0("2023-", 12:1, "-15"))

  model_timesteps <- bf$dates$midpoint

  schemes <- list(quarters,
                  firsts,
                  sort(c(firsts, fifteenths)),  # firsts and fifteenths
                  model_timesteps)  # usually equivalent to weeks


  # if necessary convert proportion of year to half proportion of year
  # (repeating each sequence twice)
  if(hpy){
    schemes <- lapply(schemes, py_to_hpy)

    # For quarters in pyear the end and start date are basically equivallent
    # resulting in duplicated date at middle of range
    # This deletes that duplicate
    schemes[[1]]  <- schemes[[1]][-6]

  }

  # Subset each scheme to just the in range values
  schemes <- lapply(schemes, function(x) select_in_range(x, range))



  # Determine the difference from the target number of elements
  diffs <- sapply(schemes, function(x) abs(target_n - length(x)))

  # Return whichever is closest in number to the target number
  return(schemes[[which.min(diffs)]])

}


