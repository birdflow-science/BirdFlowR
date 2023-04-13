#' extract distributions from BirdFlow models
#'
#' `get_distr` will return one or more distributions in their flattened form. A
#' single distribution will be returned as a vector; if multiple they will be
#' columns in a matrix.
#'
#' If the BirdFlow object has stored distributions they will be the training
#' distributions and will be returned by default unless `from_marginals = TRUE`
#' in which case distributions calculated from the marginal will be returned.
#'
#' The training distributions and the distributions calculated from the marginal
#' are very similar.
#'
#' @seealso Distributions can be passed to [predict()][predict.BirdFlow] or converted to rasters
#'   with [expand_distr()] or converted to [SpatRaster][terra::rast] with
#'   [rasterize_distr()].  [sample_distr()] will convert one cell to 1 and the rest
#'   to 0 probabilistically based on the densities in the distribution.
#' @param x a BirdFlow model
#' @param which indicates which timesteps to return. Can be one or more
#'   integers indicating timesteps; character dates in the format year-month-day
#'   e.g. `"2019-02-25"`; [`Date`][base::Dates] objects;    or `"all"` which will
#'   return distributions for all timesteps.
#' @param from_marginals if TRUE and `x` has marginals the distribution will be
#'   from the marginals even if `x` also has distributions.
#' @return either a vector with a distribution for a single timestep or a matrix
#'   with a column for each distribution.
#' @export
#'
get_distr <- function(x, which = "all", from_marginals = FALSE) {

  # Resolve which into integer timesteps
  which <- lookup_timestep(which, x)

  if(x$metadata$has_distr && !from_marginals){
    # Return stored distribution

    d <- x$distr[, which]
    if(length(which) == 1){
      attr(d, "time") <- paste0("t", which)
    }
    return(reformat_distr_labels( d, x) )

  } else {
    # Or calculate from marginals
    if(!x$metadata$has_marginals){
      if(from_marginals){
        stop("The BirdFlow model has no marginals to ",
             "calculate distribution from.")
      } else {
        stop("No distributions available in the BirdFlow object.")
      }
    }
    d <- vector(mode = "list", length = length(which))
    for(i in seq_along(which)){
      id <- which[i]  # Distribution id
      if(id == 1){  # use marginal after the distribution
        m <- x$marginals[[id]]
        d[[i]] <- Matrix::rowSums(m)
      } else { # use marginal before the distribution
        # (not available for first marginal)
        # marginal column sums are the distribution after that marginal
        m <- x$marginals[[id - 1 ]] # Prior marginal
        d[[i]] <- Matrix::colSums(m)  # colsums = distribution for state after marginal
      }
    }
    # Return single distribution as vector
    if(length(which) == 1){
      d <- d[[1]] # reformat as vector
      attr(d, "time") <- paste0("t", which)
      return(reformat_distr_labels(d, x))
    }

    # Return multiple distributions as matrix
    d <- do.call(cbind, args = d)
    dimnames(d) <- list(i = NULL, time = paste0("t", which))
    return(reformat_distr_labels(d, x))
  }
}
