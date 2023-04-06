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
#' @importFrom methods as is
#' @importMethodsFrom Matrix t %*% print
#' @importClassesFrom Matrix Matrix sparseMatrix dgCMatrix dgRMatrix
#' @importFrom stats predict
predict.BirdFlow <- function(object, distr, start, end, direction, ...) {

  # To ease transition pain
  if(!has_dynamic_mask(object))
   object <- add_dynamic_mask(object)

  dyn_mask <- object$geom$dynamic_mask

  multiple_distributions <- !is.null(dim(distr))

  if (multiple_distributions) {
    stopifnot(
      `distr should be a vector or matrix` = length(dim(distr)) == 2,
      `distr has the wrong number of rows for object` =
        dim(distr)[1] == n_active(object))
  } else {
    stopifnot(`distr length isn't right for object` =
                length(distr) == n_active(object))
  }

  # This is a sequence of transition codes to progress through
  transitions <- lookup_transitions(object, start, end, direction)
  timesteps <- as.numeric(c(gsub("^T_|-[[:digit:]]+$", "", transitions[1]),
                            gsub("^.*-", "", transitions)))

  current_dm <- dyn_mask[ , start]

  if (multiple_distributions) {
    nd <- ncol(distr) # number of distributions
    pred <- array(0,
                dim = c(n_active(object), nd, length(transitions) + 1))
    dimnames(pred) <- list(i = NULL,
                           distribution = 1:nd,
                           time = paste0("t", timesteps)
    )
    pred[, , 1] <- distr
    distr <- as(distr, "sparseMatrix")
    distr <- distr[current_dm, ]
    for (i in seq_along(transitions)) {
      tm <- get_transition(object,  transitions[i])  # transition matrix
      distr <-  tm %*%  distr
      current_dm <- dyn_mask[, timesteps[i + 1]]
      pred[ current_dm, , (i + 1)] <- as.vector(distr) # save the location
    }
    return(reformat_distr_labels(pred, object))
  }  else {  # Single distribution
    pred <- matrix(0,
                   nrow = n_active(object),
                   ncol = length(transitions) + 1)
    dimnames(pred) <- list(i = NULL, timestep = paste0("t", timesteps))
    pred[, 1] <- distr
    distr <- distr[current_dm]
    distr <- as(distr, "sparseVector")
    for (i in seq_along(transitions)){
      tm <- get_transition(object, transitions[i]) # transition matrix
      distr <- tm %*% distr
      current_dm <- dyn_mask[, timesteps[i + 1]]
      pred[current_dm, i + 1] <- as.numeric(distr) # save the location
    }
    return(reformat_distr_labels(pred, object))
  }
}

forecast <- function(x, ...) {
  stop("forecast() is deprecated. Please use predict() instead.")
}
