#' Forecast bird distributions
#'
#' forecast() projects bird distributions into the future or past
#'
#' given an initial distribution, a start time and end time generate a
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
#' @param x A BirdFlow model object
#' @param distr  a starting distribution
#' @param start,end These define the time period to forecast over. They can be
#' [Date][base::date] objects, integer timesteps, or a string with
#' "year-month-day" e.g. "2022-11-28".
#' @param direction either "forward" or "backward", only used if `start` and
#' `end` represent timesteps.
#' @return a matrix with rows for each location and columns for each timestep
#' @export
#' @importFrom Matrix Matrix
#' @importMethodsFrom Matrix t
#' @importClassesFrom Matrix Matrix sparseMatrix
forecast <- function(x, distr, start, end, direction){
  if(is.vector(distr)){
    stopifnot(`distr length isn't right for x` = length(distr) == n_active(x))
    multiple_distributions <- FALSE
  } else {
    stopifnot(
      `distr should be a vector or matrix` = length(dim(distr)) == 2,
      `distr has the wrong number of rows for x` = dim(distr)[1] == n_active(x))
    multiple_distributions <- TRUE
  }

  # This is a sequence of transition codes to progress through
  transitions <- lookup_transitions(start, end, x, direction)
  timesteps <- as.numeric(c(gsub("^T_|-[[:digit:]]+$", "", transitions[1]),
                          gsub("^.*-", "", transitions ) ) )

  if(multiple_distributions){
      nd <- ncol(distr) # number of distributions
      fc <- array(NA_real_,
                  dim = c(  n_active(x), nd, length(transitions) + 1) )
      dimnames(fc) <- list(  i = NULL,
                             distribution = 1:nd,
                             timestep = paste0("t", timesteps)
      )
      fc[ , , 1 ] <- distr
      for(i in seq_along(transitions)){
        tm <- get_transition( transitions[i], x)  # transition matrix
        distr <-  tm %*%  distr          # project
        fc[  , , (i+1)] <- as.vector(distr) # save the location
      }
      return(fc)
  }  else {  # Single distribution
      fc <- matrix(NA_real_, nrow = n_active(x), ncol = length(transitions) + 1 )
      dimnames(fc) <- list(i = NULL, timestep = paste0("t",timesteps))
      fc[ , 1] <- distr
      for(i in seq_along(transitions)){
        tm <- get_transition( transitions[i], x) # transition matrix
        distr <- tm %*% distr
        fc[ , i+1] <- as.numeric(distr) # save the location
      }
      return(fc)
  }
}
