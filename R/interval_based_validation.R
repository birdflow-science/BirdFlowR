#' Evaluate model performance using intervals (transitions data)

#' Get the interval based validation metrics for one transition pair
#'
#' @param birdflow_interval_row A row of data in the `BirdFlowIntervals` object
#' @param bf BirdFlow model
#' @param gcd Matrix of great circle distance
#' @param st_dists Matrix of S&T distribution with weeks as columns,
#' location as rows, probability as values.
#' @return A named vector with various metrics
#' \describe{
#'   \item{pred}{Weighted average great-circle distance (km) from the BF prediction distribution to the actual encounter cell}
#'   \item{st}{Weighted average great-circle distance (km) from the S\&T empirical distribution to the actual encounter cell}
#'   \item{win_prob}{Probability that BF is closer than S\&T (i.e.\ “win” probability of BF vs.\ S\&T)}
#'   \item{win_distance}{Absolute distance improvement (km): \code{st – pred}}
#'   \item{win_distance_fraction}{Normalized distance improvement: \code{(st – pred) / st}}
#'   \item{global_prob_of_the_starting}{Probability (relative abundance) of the starting cell in the BF distribution at the start date}
#'   \item{elapsed_days}{Elapsed time of the interval (days) between banding (\code{date1}) and encounter (\code{date2})}
#'   \item{elapsed_km}{Observed great-circle distance (km) between banding and encounter locations}
#'   \item{null_ll}{Log-likelihood of the encounter cell under the S\&T distribution: \code{log(final_st_distr[i_final])}}
#'   \item{ll}{Log-likelihood of the encounter cell under the BF prediction: \code{log(preds_final[i_final])}}
#'   \item{energy_score_bf}{Energy score of the BF predictive distribution (with \eqn{\beta=1}{beta=1})}
#'   \item{energy_score_st}{Energy score of the S\&T empirical distribution (with \eqn{\beta=1}{beta=1})}
#'   \item{energy_improvement}{Difference in energy score: \code{energy_score_st – energy_score_bf}}
#'   \item{pred_elapsed_dist_by_pred}{Predicted elapsed distance (km) from starting cell, weighted by BF predictions}
#'   \item{pred_elapsed_dist_by_st}{Predicted elapsed distance (km) from starting cell, weighted by S\&T distribution}
#' }
get_interval_based_validation_one_transition_pair <- function(
    birdflow_interval_row, bf, gcd, st_dists) {
  # latlong data for banding and encounter location
  point_df_initial <- data.frame(
    x = birdflow_interval_row$lon1, y = birdflow_interval_row$lat1
  )
  point_df_final <- data.frame(
    x = birdflow_interval_row$lon2, y = birdflow_interval_row$lat2
  )
  # birdflow one-hot distributions for banding and encounter locations
  d_initial <- as_distr(x = point_df_initial, bf = bf, crs = "EPSG:4326")
  # same as birdflow_interval_row$i1
  d_final <- as_distr(x = point_df_final, bf = bf, crs = "EPSG:4326")
  # same as birdflow_interval_row$i2
  # get s&t distribution for final timestep
  final_timestep <- birdflow_interval_row$timestep2
  final_st_distr <- st_dists[, final_timestep]
  # birdflow cell index for encounter location
  i_final <- which(d_final == 1)
  # birdflow predictions from banding one-hot, for encounter date
  preds <- predict(bf, d_initial,
    start = birdflow_interval_row$date1,
    end = birdflow_interval_row$date2
  )
  preds_final <- preds[, ncol(preds), drop = FALSE]
  preds_final <- as.vector(preds_final)
  # subset great circle distances for cell of actual encounter location
  gcd_final <- gcd[, i_final]
  # weighted average distance from predicted encounter
  # distribution to actual encounter location

  # Dave's distance metric
  dist_mean_pred <- sum(preds_final * gcd_final)
  dist_mean_st <- sum(final_st_distr * gcd_final)
  win_distance <- dist_mean_st - dist_mean_pred
  pred_elapsed_dist_by_pred <- sum(preds_final * gcd[, which(d_initial == 1)])
  pred_elapsed_dist_by_st <- sum(final_st_distr * gcd[, which(d_initial == 1)])

  # Normalized distance metric
  win_distance_fraction <- (dist_mean_st - dist_mean_pred) / dist_mean_st

  ## YK's function
  # For each predicted location, calculate the win probability.
  # Average the win probability based on predicted probability.
  M <- outer(gcd_final, gcd_final, FUN = function(x, y) y > x)
  win_prob_each <- rowSums(M * rep(final_st_distr, each = length(gcd_final)))
  win_prob <- sum(win_prob_each * preds_final)

  # get location index of banding starting point
  loc_i_starting <- birdflow_interval_row$i1
  date_starting <- birdflow_interval_row$timestep1

  #
  elapsed_days <- as.numeric(
    birdflow_interval_row$date2 - birdflow_interval_row$date1,
    unit = "days"
  )
  elapsed_km <- great_circle_distance_lonlat_input(
    birdflow_interval_row$lat1, birdflow_interval_row$lon1,
    birdflow_interval_row$lat2, birdflow_interval_row$lon2
  )

  # LL
  null_ll <- log(final_st_distr[i_final] + 1e-8)
  ll <- log(preds_final[i_final] + 1e-8)

  ## Energy Score Calculations (with beta = 1)
  beta <- 1
  # For the predicted distribution:
  first_term_pred <- sum(preds_final * (gcd_final^beta))
  second_term_pred <- 0.5 * sum(outer(preds_final, preds_final) * (gcd^beta))
  # Second term: weighted average of pairwise distances
  # (using full distance matrix gcd)
  energy_score_pred <- first_term_pred - second_term_pred
  # For the s&t distribution:
  first_term_st <- sum(final_st_distr * (gcd_final^beta))
  second_term_st <- 0.5 *
    sum(outer(final_st_distr, final_st_distr) * (gcd^beta))
  energy_score_st <- first_term_st - second_term_st


  # return
  return(c(
    pred = dist_mean_pred, st = dist_mean_st,
    win_prob = win_prob,
    win_distance = win_distance,
    win_distance_fraction = win_distance_fraction,
    global_prob_of_the_starting = as.numeric(
      bf$distr[loc_i_starting, date_starting] / 52
    ),
    elapsed_days = elapsed_days,
    elapsed_km = elapsed_km,
    null_ll = null_ll,
    ll = ll,
    energy_score_bf = energy_score_pred,
    energy_score_st = energy_score_st,
    energy_improvement = energy_score_st - energy_score_pred,
    pred_elapsed_dist_by_pred = pred_elapsed_dist_by_pred,
    pred_elapsed_dist_by_st = pred_elapsed_dist_by_st
  ))
}


#' Calculate interval metrics
#'
#' Calculate interval‐based validation metrics—including distance, likelihood,
#' and energy‐score metrics—for all transition pairs in a BirdFlowIntervals object.
#'
#' @param birdflow_intervals A `BirdFlowIntervals` object containing transition data
#' @param bf                 A fitted `BirdFlow` model
#'
#' @return A list with two elements:
#' \describe{
#'   \item{metrics}{A named numeric vector of summary metrics across all intervals:
#'     \describe{
#'       \item{mean_pred}{Mean weighted average distance (km) from BF predictions}
#'       \item{mean_st}{Mean weighted average distance (km) from S\&T distributions}
#'       \item{mean_win_prob}{Mean win probability (BF vs. S\&T)}
#'       \item{mean_win_distance}{Mean absolute distance improvement (km)}
#'       \item{mean_win_distance_fraction}{Mean normalized distance improvement}
#'       \item{mean_global_prob_of_the_starting}{Mean relative abundance at start cells}
#'       \item{mean_elapsed_days}{Mean elapsed days per interval}
#'       \item{mean_elapsed_km}{Mean observed great‐circle distance (km)}
#'       \item{mean_null_ll}{Mean log‐likelihood under the S\&T null distribution}
#'       \item{mean_ll}{Mean log‐likelihood under the BF prediction}
#'       \item{mean_energy_score_bf}{Mean energy score of BF predictions}
#'       \item{mean_energy_score_st}{Mean energy score of S\&T distributions}
#'       \item{mean_energy_improvement}{Mean difference in energy score}
#'       \item{mean_pred_elapsed_dist_by_pred}{Mean predicted elapsed distance by BF}
#'       \item{mean_pred_elapsed_dist_by_st}{Mean predicted elapsed distance by S\&T}
#'       \item{weighted_mean_win_prob}{Global‐abundance‐weighted mean win probability}
#'       \item{weighted_mean_win_distance}{Global‐abundance‐weighted mean win distance}
#'       \item{weighted_mean_win_distance_fraction}{Global‐abundance‐weighted mean distance fraction}
#'       \item{weighted_mean_null_ll}{Global‐abundance‐weighted mean null log‐likelihood}
#'       \item{weighted_mean_ll}{Global‐abundance‐weighted mean log‐likelihood}
#'       \item{weighted_energy_improvement}{Global‐abundance‐weighted mean energy improvement}
#'       \item{n_intervals}{Number of transition pairs evaluated}
#'     }
#'   }
#'   \item{per_interval}{A `data.frame` of the raw, per‐transition metrics (same fields as above without the “mean_” prefix)}
#' }
#' @export
calculate_interval_metrics <- function(birdflow_intervals, bf) {
  # weekly distributions directly from S&T
  st_dists <- get_distr(bf, which = "all", from_marginals = FALSE)

  # Great circle distances between cells
  gcd <- great_circle_distances(bf)

  # Calculate distance metric & ll
  dists <- sapply(
    split(birdflow_intervals$data, seq_len(nrow(birdflow_intervals$data))),
    get_interval_based_validation_one_transition_pair, bf, gcd, st_dists
  )
  dists <- t(dists)
  dists <- as.data.frame(dists)

  n_intervals <- nrow(birdflow_intervals$data)

  output <- colMeans(dists)
  names(output) <- paste0("mean_", names(output))

  output <-
    c(
      output,
      c(
        weighted_mean_win_prob = sum(
          (dists$global_prob_of_the_starting /
            sum(dists$global_prob_of_the_starting)
          ) * dists$win_prob
        ),
        weighted_mean_win_distance = sum(
          (
            dists$global_prob_of_the_starting /
              sum(dists$global_prob_of_the_starting)
          ) * dists$win_distance
        ),
        weighted_mean_win_distance_fraction = sum(
          (
            dists$global_prob_of_the_starting /
              sum(dists$global_prob_of_the_starting)
          ) * dists$win_distance_fraction
        ),
        weighted_mean_null_ll = sum(
          (dists$global_prob_of_the_starting /
            sum(dists$global_prob_of_the_starting)
          ) * dists$null_ll
        ),
        weighted_mean_ll = sum(
          (dists$global_prob_of_the_starting /
            sum(dists$global_prob_of_the_starting)
          ) * dists$ll
        ),
        weighted_energy_improvement = sum(
          (dists$global_prob_of_the_starting /
            sum(dists$global_prob_of_the_starting)
          ) * dists$energy_improvement
        ),
        n_intervals = n_intervals
      )
    )


  return(list(output, dists))
}
