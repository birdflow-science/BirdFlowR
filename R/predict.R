#' Predict bird distributions
#'
#' `predict()` projects bird distributions into the future or past. Given an
#'  initial distribution and time period specified via `...`, `predict()`
#'  generates probability distributions for each timestep.
#'
#' @param object A BirdFlow model object
#' @param distr  a starting distribution
#' @inheritDotParams lookup_timestep_sequence -x
#' @return If multiple starting distributions are input in a matrix the result
#'  will be an array with dimensions: location, distribution, and time. With one
#'  input distribution the result will be a matrix with dimensions: location
#'  and time.
#' @export
#' @importFrom Matrix Matrix
#' @importFrom methods as is
#' @importMethodsFrom Matrix t %*% print
#' @importClassesFrom Matrix Matrix sparseMatrix dgCMatrix dgRMatrix
#' @importFrom stats predict
#' @seealso
#' * [lookup_timestep_sequence()] processes the time inputs
#'   (`start`, `end`, `direction`, and `season_buffer`)
#' * [route()] and [route_migration()] are similar to `predict()` but
#'    generate routes instead of distributions.
predict.BirdFlow <- function(object, distr, ...) {


  ### BACK COMPATABILITY CODE
  object <- add_dynamic_mask(object)  # To ease transition pain

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
  transitions <- lookup_transitions(object, ...)
  timesteps <- lookup_timestep_sequence(object, ...)
  start <- timesteps[1]


  current_dm <- dyn_mask[, start]

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
      pred[current_dm, , (i + 1)] <- as.vector(distr) # save the location
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
