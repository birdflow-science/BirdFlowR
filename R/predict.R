#' Predict bird distributions
#'
#' predict() projects bird distributions into the future or past
#'
#' given an initial distribution, a start time and end time generate
#' probability distributions for each timestep between the start and end.
#'
#' @details If using dates the order of the dates are used to determine if the
#' projection is forward or backward in time and then the dates are resolved to
#' timesteps. Projections will therefore be at most one year in length and dates
#' further than a year apart may yield nonsensical results.
#'
#'  It is possible to supply multiple starting distributions as a matrix with
#'  a row for each location and a column for each distribution. In this case
#'  the result will be an array with dimensions: location, distribution,
#'  and timestep.
#'
#' @param object A BirdFlow model object
#' @param distr  a starting distribution
#' @param start,end These define the time period to predict over. They can be
#' [Date][base::date] objects, integer timesteps, or a string with
#' "year-month-day" e.g. "2022-11-28".
#' @param direction either "forward" or "backward", only used if `start` and
#' `end` represent timesteps.
#' @param ... required for consistency with generic method, but is not used.
#' @return a matrix with rows for each location and columns for each timestep
#' @export
#' @importFrom Matrix Matrix
#' @importMethodsFrom Matrix t
#' @importClassesFrom Matrix Matrix sparseMatrix
#' @importFrom stats predict
predict.BirdFlow <- function(object, distr, start, end, direction, ...){

  multiple_distributions <- !is.null(dim(distr))

  if(multiple_distributions) {
    stopifnot(
      `distr should be a vector or matrix` = length(dim(distr)) == 2,
      `distr has the wrong number of rows for object` = dim(distr)[1] == n_active(object))
  } else {
    stopifnot(`distr length isn't right for object` = length(distr) == n_active(object))
  }

  # This is a sequence of transition codes to progress through
  transitions <- lookup_transitions(object, start, end, direction)
  timesteps <- as.numeric(c(gsub("^T_|-[[:digit:]]+$", "", transitions[1]),
                            gsub("^.*-", "", transitions ) ) )

  if(multiple_distributions){
    nd <- ncol(distr) # number of distributions
    pred <- array(NA_real_,
                dim = c(  n_active(object), nd, length(transitions) + 1) )
    dimnames(pred) <- list(  i = NULL,
                           distribution = 1:nd,
                           timestep = paste0("t", timesteps)
    )
    pred[ , , 1 ] <- distr
    distr <- as(distr, "sparseMatrix")
    for(i in seq_along(transitions)){
      tm <- get_transition(object,  transitions[i])  # transition matrix
      distr <-  tm %*%  distr          # project
      pred[  , , (i+1)] <- as.vector(distr) # save the location
    }
    return(reformat_distr_labels(pred, object))
  }  else {  # Single distribution
    pred <- matrix(NA_real_, nrow = n_active(object), ncol = length(transitions) + 1 )
    dimnames(pred) <- list(i = NULL, timestep = paste0("t",timesteps))
    pred[ , 1] <- distr
    distr <- as(distr, "sparseVector")
    for(i in seq_along(transitions)){
      tm <- get_transition(object, transitions[i]) # transition matrix
      distr <- tm %*% distr
      pred[ , i+1] <- as.numeric(distr) # save the location
    }
    return(reformat_distr_labels(pred, object))
  }
}

forecast <- function(x, ...){
  stop("forecast() is deprecated. Please use predict() instead.")
}
