

#' Determine whether a points in time and space are represented in a BirdFlow
#' model.
#'
#' @param i
#' @param timestep
#' @param bf
#' @param x
#' @param y
#' @param date
#'
#' @return
#' @export
#' @examples
is_location_valid <- function(i, timestep, bf, x, y, date){

  if (missing(i)) {
    if (missing(x) || missing(y)) {
     stop("Either use argument 'i'; or both 'x' and 'y'")
    }
    i <- xy_to_i(x, y, bf)
  }

  if (missing(timestep)) {
    if(missing(date))
      stop("Need argument 'date' or 'timestep'")
    timestep <- lookup_timestep(date, bf)
  }

  stopifnot(length(i) == length(timestep))
  stopifnot(all(is.na(i) | i %in% 1:n_active(bf)))

  stopifnot(all(is.na(timestep)| timestep %in% bf$dates$interval))

  valid <- rep(TRUE, length(i)) # will hold result
  valid[is.na(i) | is.na(timestep)] <- FALSE

  # Locations are valid if the distr from the marginal is
  #  not zero for that location and timestep
  ts <- unique(timestep)
  for (t in ts) {
    d <- get_distr(bf, t, from_marginals = TRUE)
    d_not_zero  <- d != 0
    sv <- valid &  timestep == t
    valid[sv] <- d_not_zero[i[sv]]
  }
  return(valid)
}
