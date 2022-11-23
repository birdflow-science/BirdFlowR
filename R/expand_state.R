#' Convert a collapsed model state into an expanded state
#'
#' This function converts a collapsed state in which the first dimension
#' represents all the active cells in the model into an expanded state in which
#' the first two dimensions represent the locations of the cell in physical
#' space (row, col) and are thus suitable for plotting or conversion into
#' spatial objects.
#'
#' In its collapsed form a state vector represents the state of a model at one
#' point in time as a probability distribution. Each value can be interpreted as
#' the relative abundance or probability of finding a bird at the corresponding
#' location. A special case is when the vector has mostly 0's and a single 1 in
#' which case it represents a single position of a bird or group of birds.
#' Additional dimensions can represent multiple timesteps and/or multiple
#' individual birds, model runs etc.
#'
#' The location information in the collapsed state is not easily accessible as
#' position in the vector is ordered based on row dominant unmasked cells in
#' landscape and R uses column dominant order.
#'
#' @param x Either a vector with one value per active cell in the model or a
#'   matrix with a row for each active cell and a column for each state, or
#'   possible a higher dimension object.  In all cases the first dimension is
#'   for states in the model.
#' @param geom The geometry component of a BirdFlow model. For convenience a
#'   full BirdFlow model can be passed as well. The `mask`, `nrow`, and
#'   `ncol` elements are used.
#' @return an expanded version of `x` with one additional dimension in which
#'   the first two dimensions represent rows and columns in space and
#'   replace the first dimension in the input.
#' @export
expand_state <- function(x, geom){d
  # The expansion is awkward because the collapsed state is a subset of the full
  # matrix in row dominant order but R will assign in column dominant order.
  # This is resolved by switching the order of the first two dimension to fill
  # and then switching back. If state was always a vector the return value would
  # be a matrix and the "switch" would be transposing.

  if("geom" %in% names(geom)) # Allows passing a full BirdFlow object as geom
    geom <- geom$geom

  # Check for proper number of active cells
  n_active <- sum(geom$mask)
  x <- as.array(x) # so dim will work even when 1 dimensional.
  if(!dim(x)[1] == n_active)
    stop("Expected first (possibly only) dimension of x to represent ",
         n_active, " active cells. Found ", dim(x)[1], ".")

  # Create empty array with first two dimensions switched:
  a <- array(NA, dim = c(geom$ncol, geom$nrow, dim(x)[-1])) # col, row, ...
  a[t(geom$mask)] <- as.vector(x) # fill
  perm <- 1:length(dim(a))
  perm[1:2] <- 2:1
  a <- aperm(a, perm = perm) # switch to row, col, ...
  return(a)

  # # Older code that only handles one dimensional input
  # # and is slightly easier to interpret:
  # # Converts vector to matrix
  # if(is.vector(x)){
  #   if(length(x) != n_active)
  #     stop("State should have", n_active, "values.")
  #   m <- matrix(nrow = geom$ncol , ncol = geom$nrow) # reversed on purpose
  #   m[t(geom$mask)] <- x # fills by columns which represent rows
  #   return(t(m))
  # }

}
