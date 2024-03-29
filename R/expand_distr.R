#' Convert a collapsed distribution into a expanded, raster equivalent
#'
#' This function converts a collapsed distribution in which the first (and
#' possibly only) dimension represents all the locations in the model into an
#' expanded, raster state in which the first two dimensions represent the
#' locations of the cell in physical space (row, col) and are thus suitable for
#' plotting or conversion into spatial objects.
#'
#' In its collapsed form a single distribution is stored as a vector. Each value
#' can be interpreted as the relative abundance or probability of finding a bird
#' at the corresponding location. A special case is when the vector has mostly
#' 0's and a single 1 in which case it represents a single position of a bird or
#' group of birds, a very concentrated distribution.
#'
#' Additional dimensions can represent multiple timesteps and/or multiple
#' individual birds, model runs etc.
#'
#' The location information in the collapsed distribution is not easily
#' accessible as position in the vector is ordered based on row-major ordered
#' unmasked cells in the extent, and R uses column-major order.
#'
#' As of May 2023 this is now an internal function replaced by
#'  [rasterize_distr(format = "numeric")][rasterize_distr()].
#'
#' @param distr Either a vector representing a single distribution with one
#'   value per location in the model or a matrix in which each column is such a
#'   vector. Higher dimensions are allowed (but unlikely); in all cases the
#'   first dimension is for locations in the model.
#' @param bf A BirdFlow model
#' @return An expanded version of `distr` with one additional dimension, in
#'   which the first two dimensions are rows and columns in space (a raster) and
#'   replace the first dimension in the input.
#' @keywords internal
expand_distr <- function(distr, bf) {
  # The expansion is awkward because the collapsed state is a subset of the full
  # matrix in row-major order but R will assign in column-major order.
  # This is resolved by switching the order of the first two dimension to fill
  # and then switching back. If state was always a vector the return value would
  # be a matrix and the "switch" would be transposing.

  # Check for proper number of active cells
  distr <- as.array(distr) # so dim will work even when 1 dimensional.
  if (!dim(distr)[1] == n_active(bf))
    stop("Expected first (possibly only) dimension of distr to represent ",
         n_active(bf), " active cells. Found ", dim(distr)[1], ".")

  # Create empty array for result but with first two dimensions switched
  # so that it is filling is by row instead of column.
  a <- array(NA, dim = c(ncol(bf), nrow(bf), dim(distr)[-1])) # col, row, ...
  a[t(bf$geom$mask)] <- as.vector(distr) # fill
  perm <- seq_len(length(dim(a)))
  perm[1:2] <- 2:1
  a <- aperm(a, perm = perm) # permute the array so first two dimensions are
                             # row, col

  dimnames(a) <- c(list(row = NULL, col = NULL), dimnames(distr)[2])

  if (length(dim(a)) == 2 && !is.null(attr(distr, "time"))) {
    attr(a, "time") <- attr(distr, "time")
  }

  return(a)

  # nolint start: commented_code_linter.
  # # Code that only handles one dimensional input
  # # and is slightly easier to interpret:
  # # Converts vector to matrix
  # if(is.vector(distr)){
  #   if(length(distr) != n_active(bf))
  #     stop("State should have", n_active(bf), "values.")
  #   m <- matrix(nrow = ncol(bf) , ncol = nrow(bf)) # reversed on purpose
  #   m[t(bf$geom$mask)] <- distr # fills by columns which represent rows
  #   return(t(m))
  # }
  # nolint end

}
