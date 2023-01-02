#' extract distributions from BirdFlow models
#'
#' This will return one or more distributions in their compact vector forms. If
#' more than one they will be columns in a matrix. If the BirdFlow object has
#' a `distr` component it will return those otherwise it will calculate
#' the distributions from the `marginals` (not implemented yet).
#'
#' @seealso Distributions can be passed to [forecast] or converted to rasters with
#' [expand_distr] or converted to [terra::rast][SpatRaster] with
#' [rasterize_distr].  [sample_distr] will convert one cell to 1 and the rest to
#' 0 probabilistically based on the densities in the distribution.
#' @param which indicates which distributions to return. Can be one or more
#' integers indicating timesteps, or character dates in the format
#' year-month-day e.g. "2019-02-25", or \code{\link[base::Dates]{Dates}}
#' @param obj a BirdFlow model
#' @return either a vector with a distribution for a single timestep or
#'   a matrix with a column for each distribution.
#' @export
#'
#' @examples
get_distr <- function(which, obj){

  if(is.character(which)){
    if(which == "all"){
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

  if(obj$metadata$has_distr){
    return(obj$distr[, which])
  } else {
    stop("Getting distributions from marginals hasn't been implemented yet.")
  }
}
