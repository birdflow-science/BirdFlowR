
#' Evaluate BirdFlow model performance
#'
#' DEPRECATED FUNCTION.  Please use [distribution_performance()] instead.
#'
#' Calculate several the correlation between projected distributions and
#' the eBird Status and Trends (S&T) distributions used to train the BirdFlow
#' model.
#'
#' @details  "Training distribution" is used to describe the eBird S&T
#' distributions used to train the BirdFlow models.  "Marginal distribution"
#' describes a distribution calculated from row or column sums of a marginal,
#' joint probability matrix. "Projected distribution" is used to describe
#' a training distribution that is multiplied with a transition matrix
#' (derived from the marginal distribution) to project forward one timestep;
#' or multiplied repeatedly with transition matrices to project forward
#' multiple timesteps.
#'
#' Correlations are calculated for only the non-dynamically masked cells.
#'
#' @param x A BirdFlow object
#' @param distr_only set to TRUE to calculate only `mean_distr_cor` and
#' `min_distr_cor` metrics.
#'
#' @return
#' \describe{
#' \item{mean_step_cor}{Indicates on average how well the model project a
#' single timestep. The mean correlation, across all timesteps, between the
#' training (eBird S&T) distribution projected forward one timestep, and the
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
#'  training distribution. The minimum observed correlation between a marginal
#'  and training distribution.}
#' }
#' @keywords internal
#' @export
evaluate_performance <- function(x, distr_only = FALSE) {

  # Note this is now a placeholder wrapper to distribution_performance()

  warning("evaluate_performance is deprecated. ",
          "Please use distribution_performance() instead.")

  if (distr_only) {
    res <- distribution_performance(x, metrics = c("mean_distr_cor",
                                                   "min_distr_cor"))
    return(res[c("mean_distr_cor", "min_distr_cor")])
  }


  res <- distribution_performance(x)
  res$md_traverse_cor <- NULL
  names(res)[names(res) == "st_traverse_cor"] <- "traverse_cor"

  # enforce original metric order
  metric_names <- c("mean_step_cor", "min_step_cor", "traverse_cor",
                    "mean_distr_cor", "min_distr_cor")

  stopifnot(all(metric_names %in% names(res)))
  return(res[metric_names])

}
