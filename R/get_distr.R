#' extract distributions from BirdFlow models
#'
#' `get_distr` will return one or more distributions in their compact form.
#' A single distribution will be returned as a vector; if multiple they will be
#' columns in a matrix.
#'
#' If the BirdFlow object has stored distributions they will be the training
#' distributions and will be returned by default unless `from_marginals = TRUE`
#' in which case distributions calculated from the marginal will be returned.
#'
#' The training distributions and the distributions calculated from the
#' marginal are very similar.
#'
#' @seealso Distributions can be passed to [forecast] or converted to rasters
#'   with [expand_distr] or converted to [SpatRaster][terra::rast] with
#'   [rasterize_distr].  [sample_distr] will convert one cell to 1 and the rest
#'   to 0 probabilistically based on the densities in the distribution.
#' @param which indicates which distributions to return. Can be one or more
#'   integers indicating timesteps; character dates in the format
#'   year-month-day e.g. `"2019-02-25"`; [`Date`][base::Dates] objects; or
#'   `"all"` which will return distributions for all timesteps.
#' @param obj a BirdFlow model
#' @param from_marginals if TRUE and `obj` has marginals the distribution will
#'   be from the marginals even if `obj` also has distributions.
#' @return either a vector with a distribution for a single timestep or a matrix
#'   with a column for each distribution.
#' @export
#'
#' @examples
get_distr <- function(which, obj, from_marginals = FALSE){

  # Resolve which into integer timesteps
  if(is.character(which)){
    if(length(which) == 1 && which == "all"){
      which <- obj$dates$interval
    } else {
      which <- lubridate::as_date(which)
    }
  }
  if(lubridate::is.Date(which)){
    doy <- lubridate::yday(which)+0.5
    centers <- obj$dates$doy
    breaks <- c(-Inf, (centers[-1] + centers[-length(centers)])/2, Inf )
    which <- findInterval(doy, breaks)
  }
  if(!all(which %in% obj$dates$interval) ){
    wrong <- setdiff(which, obj$dates$interval)
    stop("which resolved to timesteps that aren't in the model:",
         paste(wrong, collapse = ", "))
  }


  if(obj$metadata$has_distr && !from_marginals){ # Return stored distribution
    return(obj$distr[, which])
  } else { # Or calculate from marginals
    if(!obj$metadata$has_marginals){
      if(from_marginals){
        stop("The BirdFlow model has no marginals to calculate distribution from.")
      } else {
        stop("No distributions available in the BirdFlow object.")
      }
    }
    d <- vector(mode = "list", length = length(which))
    for(i in seq_along(which)){
      id <- which[i]  # Distribution id
      if(id == 1){  # use marginal after the distribution
        m <- obj$marginals[[id]]
        d[[i]] <- rowSums(m)
      } else { # use marginal before the distribution
        # (not available for first marginal)
        # marginal column sums are the distribution after that marginal
        m <- obj$marginals[[id - 1 ]] # Prior marginal
        d[[i]] <- colSums(m)  # colsums = distribution for state after marginal
      }
    }
    # Return single distribution as vector
    if(length(which) == 1)
      return(d[[1]])
    # Return multiple distributions as matrix
    d <- do.call(cbind, args = d)
    dimnames(d) <- list(i = NULL, timestep = paste0("t", which))
    return(d)
  }
}
