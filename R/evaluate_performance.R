
#' Evaluate BirdFlow model performance
#'
#' Calculate several the correlation between projected distributions and
#' the ebirdst distributions used to train the BirdFlow model.
#'
#' @details  "Training distribution" is used to describe the ebirdst
#' distributions used to train the BirdFlow models.  "Marginal distribution"
#' describes a distribution calculated from row or column sums of a marginal,
#' joint probability matrix. "Projected distribution" is used to describe
#' a training distribution that is multiplied with a transition matrix
#' (derived from the marginal distribution) to project forward one timestep;
#' or multiplied repeatedly with transition matrices to project forward
#' multiple timesteps.
#'
#' @param bf A BirdFlow object
#'
#' @return
#' \describe{
#' \item{mean_step_cor}{Indicates on average how well the model project a
#' single timestep. The mean correlation, across all timesteps, between the
#' training (ebirdst) distribution projected forward one timestep, and the
#' training distribution for the projected timestep.
#' }
#' \item{min_step_cor}{Indicates the quality of the worst single step
#' projection. The minimum correlation (across all timesteps) between
#' a single step projection each training distribution and the training
#' distribution for the projected timestep.}
#' \item{traverse_cor}{Indicates how well does the model project through all
#' timesteps. The correlation between last distribution projected iteratively
#' from the fist training distribution; and the last training distribution. }
#' \item{mean_distr_cor}{Indicates on average how well the marginal preserves
#' the training distributions. The mean correlation between the training
#' distributions and distributions calculated from the marginals.}
#'  \item{min_distr_cor}{Indicates how well the poorest marginal preserves the
#'  training distribution. The minium observed coorlation between a marginal and
#'  training distribution.}
#' }
#' @export
evaluate_perfomance <- function(bf){

  transitions <- lookup_transitions(start = 1, end = n_distr(bf), bf)
  distr_cor <- single_step_cor <- numeric(length(transitions))
  for(i in seq_along(transitions)){
    from <- as.numeric(gsub("^T_|-[[:digit:]]+$", "", transitions[i]))
    to <- as.numeric(gsub("^T_[[:digit:]]+-", "", transitions[i]))
    start_distr <- get_distr(from, bf, from_marginals = FALSE)
    end_distr <- get_distr(to, bf, from_marginals = FALSE)
    projected <- forecast(bf,distr = start_distr, start = from, end = to)
    single_step_cor[i] <- cor(end_distr, projected[, ncol(projected)])
    marginal_start_distr <- get_distr(from, bf = bf, from_marginals = TRUE)
    distr_cor[i] <- cor(start_distr, marginal_start_distr)
  }

  start <- 1
  end <- n_distr(bf)
  start_distr <- get_distr(1, bf, from_marginals = FALSE)
  end_distr <- get_distr(end, bf, from_marginals = FALSE)
  projected <- forecast(bf, start_distr, start, end, "forward")
  projected <- projected[, ncol(projected)] # end
  traverse_cor <- cor(start_distr, projected)

  return(list(mean_step_cor = mean(single_step_cor),
              min_step_cor = min(single_step_cor),
              traverse_cor = traverse_cor,
              mean_distr_cor = mean(distr_cor),
              min_distr_cor = min(distr_cor) ) )

}