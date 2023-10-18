#' Are locations and distributions covered by the model for the given time
#'
#' A location or and time is valid if the model has a non zero probability  of
#' movement into and out of that location at that time. An invalid state
#' either has zero probability of movement into and out of it, or isn't covered
#' by the model at all. Similarly distributions are valid if all of the
#' probability is in cells for which movement is modeled.
#'
#' For `is_location_valid()` there are 2 ways of inputting locations. Only
#' one should be used: either `i` a vector of location indices
#' (see [xy_to_i()]), or `x` and `y`.
#'
#' For both functions time should be input either as `timestep` or `date`.
#'
#' The number of timesteps or dates should either be 1 or match to the number
#' of locations or distributions; if singular it will be applied to all.
#'
#' A location is invalid if any of the following apply:
#'  1. The location isn't in the model extent
#'  2. The location doesn't correspond to an active cell in the model;
#'   it's masked out by the static mask.
#'  3. The location isn't a valid state at the given timestep or date; it's
#'    excluded by the dynamic mask or by state or other sparsification see
#'     [sparsify()].
#'  4. The timestep isn't valid, or the date doesn't have an associated
#'  timestep. The second applies only to BirdFlow models that don't cover the
#'   whole year.
#'
#' A distribution is invalid for similar reasons but applied to all the
#' locations that have non-zero values.
#'
#' @param bf a BirdFlow object
#' @param i state space index (location)
#' @param distr one or more distributions in vector or matrix form representing
#'   a probability for each active cell in the model.
#' @param x x coordinates in the `bf`'s CRS ([crs(bf)][terra::crs()])
#' @param y y coordinate
#' @param timestep the timestep
#' @param date date in any format accepted by [lookup_timestep()]
#' @param return_mask if TRUE return a mask with the same dimensions as `distr`
#' which is TRUE for all cells that can have valid non-zero values. This is
#' conditioned on the timestep associated with each distribution.
#' @return a logical vector, 'TRUE` if valid `FALSE` otherwise with values for
#' each input location or distribution. See `return_mask` for an exception.
#' @export
#' @examples
#'
#' bf <- BirdFlowModels::amewoo
#' timestep  <- 3
#'
#' # Sample two valid locations from a distribution
#' distr <- get_distr(bf, timestep, from_marginals = TRUE)
#' locs <- sample_distr(distr, n = 2)
#' i <- apply(locs, 2, function(x) which(as.logical(x)))
#' is_location_valid(bf, i, timestep = timestep)
#'
#' # Sample a few invalid locations
#' i <- sample(which(distr == 0), 2)
#' is_location_valid(bf, i, timestep = timestep)
#'
is_location_valid <- function(bf, i, x, y, timestep, date) {

  if (missing(i) && (missing(x) || missing(y)))
    stop("Either use argument 'i', or both 'x' and 'y'.")
  if (!missing(i) && (!missing(x) || !missing(y)))
    stop("Use only one way of specifying locations (i, or x and y).")
  if (!missing(x) || !missing(y))
    i <- xy_to_i(x, y, bf)

  if (missing(timestep)) {
    if (missing(date))
      stop("Need argument 'date' or 'timestep'")
    timestep <- lookup_timestep(date, bf)
  }

  if (length(timestep) == 1)
    timestep <- rep(timestep, length(i))
  stopifnot(length(i) == length(timestep))
  stopifnot(all(is.na(i) | i %in% 1:n_active(bf)))

  stopifnot(all(is.na(timestep) | timestep %in% bf$dates$interval))
  valid <- rep(TRUE, length(timestep)) # will hold result
  valid[is.na(timestep)] <- FALSE
  ut <- unique(timestep)

  valid[is.na(i)] <- FALSE
  # Locations are valid if the distr from the marginal is
  #  not zero for that location and timestep
  for (t in ut) {
    d <- get_distr(bf, t, from_marginals = TRUE)
    d_not_zero  <- d != 0
    sv <- valid &  timestep == t
    valid[sv] <- d_not_zero[i[sv]]
  }
  return(valid)
}


#' @export
#' @rdname is_location_valid
is_distr_valid <- function(bf, distr, timestep, date, return_mask = FALSE) {

  if (!is.matrix(distr))
    distr <- matrix(distr, ncol = 1)

  if (missing(timestep)) {
    if (missing(date))
      stop("Need argument 'date' or 'timestep'")
    timestep <- lookup_timestep(date, bf)
  }

  if (length(timestep) == 1)
    timestep <- rep(timestep, ncol(distr))
  stopifnot(ncol(distr) == length(timestep))

  if (!all(is.na(timestep) | timestep %in% bf$dates$interval))
    stop("Invalid timesteps - they aren't all in the model.")

  valid <- rep(TRUE, length(timestep)) # will hold result
  valid[is.na(timestep)] <- FALSE
  valid[apply(distr, 2, anyNA)] <- FALSE

  # Here we evaluate whether every cell of the input distribution that has value
  # (isn't zero) corresponds to a cell that has value in the BirdFlow
  #  distribution (extracted from the marginal) for the same timestep
  # The mask in this case is a dynamic mask with the same dimensions as distr
  # and where each column is based on the timestep associated with the distr
  # column.
  ut <- unique(timestep)
  mask <- matrix(FALSE, nrow = nrow(distr), ncol = ncol(distr))
  for (t in ut) {
    sv <- valid &  timestep == t
    bfd <- get_distr(bf, t, from_marginals = TRUE) # bird flow distribution
    bf_has_value  <- bfd != 0  # is the bird flow distribution zero
    mask[, sv] <- bf_has_value
  }

  if (return_mask) {
    if (ncol(mask) == 1)
      mask <- as.vector(mask)
    return(mask)
  }

  # These objects have the same dimensions as distr with corresponding cells
  distr_has_value <- distr != 0 # TRUE if cell in the distr has value
  valid_cells <- matrix(TRUE, nrow = nrow(distr), ncol = ncol(distr))
  valid_cells[distr_has_value & !mask] <- FALSE
  valid <- apply(valid_cells, 2, all)

  return(valid)
}
