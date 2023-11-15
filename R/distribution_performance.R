
#' Evaluate a BirdFlow models ability to recreate training distributions
#'
#' Calculate the correlation between model derived distributions and the eBird
#' Status and Trend distributions used to train the BirdFlow model.
#'
#' @details BirdFlow models are trained on eBird Status and Trends distributions
#'   ("Training distributions"). "Marginal distribution" describes a
#'   distribution calculated from row or column sums of a marginal, joint
#'   probability matrix (also part of the model). "Projected distribution" is
#'   used to describe a distribution that is multiplied with a transition matrix
#'   (derived from the marginal distribution) to project forward one timestep
#'   or multiplied repeatedly with a series of transition matrices to project
#'   forward multiple timesteps
#'
#'   Correlations are calculated for only the non-dynamically masked cells.
#'
#'   The `...` argument can be used to define a subset of time to evaluate over
#'   in which case all metrics will be calculated only on that subset. The
#'   default is to use all timesteps.
#'
#' @param x A BirdFlow object
#' @param metrics If NULL calculate all metrics.  Otherwise set to a subset of
#'   the metric names to calculate only those metrics.
#' @inheritDotParams lookup_timestep_sequence -x
#'   n
#' @return
#' \describe{
#' \item{mean_step_cor}{Indicating on average how well the model projects a
#' single timestep, `mean_step_cor` is the mean correlation, across all
#' timesteps, between the
#' training (eBird Status and Trends) distribution projected forward one
#' timestep, and the training distribution for that projected timestep.
#' }
#' \item{min_step_cor}{Indicates the quality of the worst single step
#' projection. The minimum correlation (across all timesteps) between
#' a single step projection of the training distribution and the training
#' distribution for the projected timestep.}
#'
#' \item{mean_distr_cor}{Indicates on average how well the marginal preserves
#' the training distributions. The mean correlation between the training
#' distributions and distributions calculated from the marginals.}
#'  \item{min_distr_cor}{Indicates how well the poorest marginal preserves the
#'  training distribution. The minimum observed correlation between a marginal
#'  and training distribution.}
#'  \item{st_traverse_cor, md_traverse_cor}{Indicates how well the model
#'  projects a distribution through multiple timesteps. They are the correlation
#'  between last distribution in a series projected iteratively forward from the
#'  fist distribution; and the eBird Status and Trends (training) distribution
#'  for the last timestep. `st_traverse_cor` starts with the first timestep
#'  Status and Trends (st) distribution, while `md_traverse_cor` starts with a
#'  the marginal distribution for the same timestep;  both versions compare the
#'  projected distribution to the eBird Status and Trends distribution for same
#'  timestep.}
#'
#' }
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' distribution_performance(bf)
#'
#' # Just for prebreeding_migration
#' distribution_performance(bf, season = "prebreeding_migration")
#'
#' @export
distribution_performance <- function(x, metrics = NULL, ...) {

  stopifnot(inherits(x, "BirdFlow"))


  ### Transition code
  if (!has_dynamic_mask(x))
    x <- add_dynamic_mask(x)

  all_metrics <- c("min_step_cor",
                   "mean_step_cor",
                   "min_distr_cor",
                   "mean_distr_cor",
                   "st_traverse_cor",
                   "md_traverse_cor")

  if (is.null(metrics))
    metrics <- all_metrics

  if (!all(metrics %in% all_metrics)) {
    stop('Metrics should be NULL or one or more of:, "',
         paste(all_metrics, collapse = '", "'), '"')
  }

  # Make order consistent with "all_metrics" order
  metrics <- all_metrics[all_metrics %in% metrics]

  # Initiate all metrics as NA
  st_traverse_cor <- md_traverse_cor <- NA_real_
  min_step_cor <- mean_step_cor <- NA_real_
  min_distr_cor <- mean_distr_cor <- NA_real_

  # Flags to control what is calculated
  do_distr <- any(c("min_distr_cor", "mean_distr_cor") %in% metrics)
  do_traverse <- any(grepl("traverse", metrics))
  do_step <- any(grepl("_step_", metrics))

  # Transitions to evaluate over
  transitions <- lookup_transitions(x, ...)
  timesteps <- lookup_timestep_sequence(x, ...)
  start <- timesteps[1]
  end <- timesteps[length(timesteps)]

  # Calculate single step and distr metrics
  if (do_distr || do_step) {

    distr_cor <- single_step_cor <- numeric(length(transitions))

    for (i in seq_along(transitions)) {

      # Calculate distribution correlations (marginal vs S&T for each step)
      from <- timesteps[i]
      to <- timesteps[i + 1]
      start_distr <- get_distr(x, from, from_marginals = FALSE)
      marginal_start_distr <- get_distr(x, from, from_marginals = TRUE)
      start_dm <- get_dynamic_mask(x, from)
      distr_cor[i] <- cor(start_distr[start_dm], marginal_start_distr[start_dm])

      # Calculate single step projection correlations
      if (do_step) {
        end_distr <- get_distr(x, to, from_marginals = FALSE)
        projected <- predict(x, distr = start_distr, start = from, end = to)
        end_dm <- get_dynamic_mask(x, to) # end dynamic mask
        single_step_cor[i] <- cor(end_distr[end_dm],
                                  projected[end_dm, ncol(projected)])
      }
    } # end loop through transitions

    if (do_step) {
      mean_step_cor <- mean(single_step_cor)
      min_step_cor <- min(single_step_cor)
    }
    if (do_distr) {
      mean_distr_cor <- mean(distr_cor)
      min_distr_cor <- min(distr_cor)
    }
  } # end distr and step

  # Calculate Traverse Correlation
  if (do_traverse) {

    start_distr <- cbind(get_distr(x, start, from_marginals = FALSE), # st_
                         get_distr(x, start, from_marginals = TRUE))  # md_


    end_distr <- get_distr(x, end, from_marginals = FALSE)
    projected <- predict(x, distr =  start_distr, start =  start,
                         end =  end, direction =  "forward")

    projected <- projected[, , dim(projected)[3]] # subset to last timestep
    end_dm <- get_dynamic_mask(x, end)
    # Two traverse correlations
    # "st_" starts with eBird S&T distribution
    st_traverse_cor <- cor(end_distr[end_dm], projected[end_dm, 1])
    # "md_" starts with marginal distribution
    md_traverse_cor <- cor(end_distr[end_dm], projected[end_dm, 2])
  } # end traverse

  result <- list(mean_step_cor = mean_step_cor,
                 min_step_cor =  min_step_cor,
                 st_traverse_cor = st_traverse_cor,
                 md_traverse_cor = md_traverse_cor,
                 mean_distr_cor = mean_distr_cor,
                 min_distr_cor = min_distr_cor)

  return(result[metrics])

}
