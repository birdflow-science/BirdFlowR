
#' Retrieve date associated with timesteps, transitions, or marginals
#'
#' @param x A vector of one of the following formats:
#' 1. Integer between 1 and `n_timesteps(bf)` representing timestep.
#' 2. Character with "T" followed by digits that indicate timesteps,
#'  (this format is used internally to label timestep dimensions of objects)
#' 3. Marginal or Transition names. These start with either "T_" or "M_", and
#' then have two timesteps respresented by digits and separated by a dash,
#'  E.g. "T_01-02".
#' @param bf A BirdFlow object
#' @param timestep Deprecated alternative to `x`.  Previous versions of
#' `lookup_dates()` only supported timestep input and used `timestep` as
#' the first argument.
#' @return A Date object
#' @seealso [get_dates()], [lookup_timestep()], [lookup_timestep_sequence()]
#' @export
#'
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' lookup_date(1:5, bf)
lookup_date <- function(x, bf, timestep = NULL) {

  if(all(is.na(x)))
    return(as.Date(x))

  if(!is.null(timestep)){
    x <- timestep
    warning("timestep argument to lookup_dates() is deprecated please use x instead")
  }

  if(is.character(x)){
    if(all(grepl("^T[[:digit:]]+$", x[!is.na(x)]))){
      # There are character representations of timesteps used to
      # label distributions internally
      x <- as.numeric(gsub("^T", "", x))
      # x is now numeric timeseps and will be handled at bottom

    } else if (all(grepl("^[MT]_[[:digit:]]+-[[:digit:]]+$", x[!is.na(x)]))){

      # These are marginal or transition IDs in the form M_01-02 or T_52-01
      # We'll convert them to two columns of dates represnting the
      # first and second timestep.
      # Then we'll return the average date.
      # For transitions over the year boundary (loops == TRUE)
      # We add a year to the date associated with timestep 1 so that
      # the two dates are only a week apart.
      # At the end we coherse all dates to have the ebirdst version year
      # (potentially removing the added year)

      ts_pairs <- gsub("^[MT]_([[:digit:]]+)-([[:digit:]]+)$",
                       "\\1,\\2", x, perl = TRUE)
      ts_pairs <- do.call(rbind, args = strsplit(ts_pairs, ","))
      storage.mode(ts_pairs) <- "numeric"
      loops <- !(ts_pairs[, 2] - ts_pairs[, 1]) %in% c(1, -1)
      dates <- data.frame(first = lookup_date(ts_pairs[, 1], bf),
                          second = lookup_date(ts_pairs[, 2], bf))
      # For rows that loop add a year to the date associated with the first
      # timestep
      # Backward loop over year boundary:
      sv <- loops & ts_pairs[, 1] == 1 & !is.na(x)
      if(any(sv))
        lubridate::year(dates[sv, 1]) <- lubridate::year(dates[sv, 1]) + 1
      # Forward loop over year boundary:
      sv <- loops & ts_pairs[, 2] == 1  & !is.na(x)
      if(any(sv))
        lubridate::year(dates[sv, 2]) <- lubridate::year(dates[sv, 2]) + 1

      # Mean date
      mean_date <- apply(dates, 1, function(x) mean(as.Date(x))) |>
        as.Date(origin = "1970-01-01")

      # Restore original year
      lubridate::year(mean_date) <- get_metadata(bf, "ebird_version_year")

      return(lubridate::as_date(mean_date))
    } else if(all(grepl("^[[:digit:]]+$", x[!is.na(x)]))) {
      # All digits but as text - assume timesteps
      x <- as.numeric(x)
    } else {
      # Unrecognized character input
      stop("Unrecognized character input to lookup_date(). ",
           "Valid inputs are numeric timesteps, timesteps preceeded by a \"T\", ",
           "marginal names, and transition names.", call. = FALSE)

    }
  } # end character input

  stopifnot(is.numeric(x))
  # Numeric (timestep) input
  dates <- get_dates(bf)
  lubridate::as_date(dates$date[match(x, dates$timestep)])
}
