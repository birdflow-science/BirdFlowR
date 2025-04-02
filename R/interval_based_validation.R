#' Calculate the interval based validation, including predictive distance metric, log likelihood, and log likelihood baseline
#' For distance metrics: What is the distance and probability that BirdFlow model prediction beats the naive S&T probability distribution-based prediction?

#' Get the interval based validation metrics for one transition pair
#'
#' @param birdflow_interval_row A row of data in the BirdFlowIntervals object
#' @param bf BirdFlow model
#' @param gcd Matrix of great circle distance
#' @param st_dists Matrix of S&T distribution with weeks as columns, 
#' location as rows, probability as values.
#'
#' @return A named vector with distance metrics
#' @export
get_interval_based_validation_one_transition_pair <- function(birdflow_interval_row, bf, gcd, st_dists){
  
  # latlong data for banding and encounter location
  point_df_initial <- data.frame(x = birdflow_interval_row$lon1, y = birdflow_interval_row$lat1)
  point_df_final   <- data.frame(x = birdflow_interval_row$lon2, y = birdflow_interval_row$lat2)
  # birdflow one-hot distributions for banding and encounter locations
  d_initial <- as_distr(x = point_df_initial, bf = bf, crs = 'EPSG:4326') # same as birdflow_interval_row$i1
  d_final <- as_distr(x = point_df_final, bf = bf, crs = 'EPSG:4326') # same as birdflow_interval_row$i2
  # get s&t distribution for final timestep
  final_timestep <- birdflow_interval_row$timestep2
  final_st_distr <- st_dists[,final_timestep]
  # birdflow cell index for encounter location
  i_final <- which(d_final == 1)
  # birdflow predictions from banding one-hot, for encounter date
  preds <- predict(bf, d_initial, start = birdflow_interval_row$date1, end = birdflow_interval_row$date2)
  preds_final <- preds[,ncol(preds),drop = FALSE]
  preds_final <- as.vector(preds_final)
  # subset great circle distances for cell of actual encounter location
  gcd_final <- gcd[,i_final]
  # weighted average distance from predicted encounter distribution to actual encounter location
  
  # Dave's distance metric
  dist_mean_pred <- sum(preds_final * gcd_final)
  dist_mean_st   <- sum(final_st_distr * gcd_final)
  win_distance <- dist_mean_st - dist_mean_pred
  pred_elapsed_dist_by_pred <- sum(preds_final * gcd[,which(d_initial == 1)])
  pred_elapsed_dist_by_st <- sum(final_st_distr * gcd[,which(d_initial == 1)])
  
  # Normalized distance metric
  win_distance_fraction = (dist_mean_st - dist_mean_pred) / dist_mean_st
  
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
  elapsed_days <- as.numeric(birdflow_interval_row$date2 - birdflow_interval_row$date1, unit='days')
  elapsed_km <- great_circle_distance_lonlat_input(birdflow_interval_row$lat1, birdflow_interval_row$lon1,
                                                        birdflow_interval_row$lat2, birdflow_interval_row$lon2)
  
  # LL
  null_ll <- log(final_st_distr[i_final] + 1e-8)
  ll <- log(preds_final[i_final] + 1e-8)

  # effective_win_distance
  effective_win_distance <- dist_mean_st/exp(null_ll) - dist_mean_pred/exp(ll)
  
  ## Energy Score Calculations (with beta = 1)
  beta <- 1
  # For the predicted distribution:
  first_term_pred <- sum(preds_final * (gcd_final^beta))
  second_term_pred <- 0.5 * sum(outer(preds_final, preds_final) * (gcd^beta)) #Second term: weighted average of pairwise distances (using full distance matrix gcd)
  energy_score_pred <- first_term_pred - second_term_pred
  # For the s&t distribution:
  first_term_st <- sum(final_st_distr * (gcd_final^beta))
  second_term_st <- 0.5 * sum(outer(final_st_distr, final_st_distr) * (gcd^beta))
  energy_score_st <- first_term_st - second_term_st
  
  
  # return
  return(c(pred = dist_mean_pred, st = dist_mean_st, 
    win_prob = win_prob,
    win_distance = win_distance,
    win_distance_fraction = (dist_mean_st - dist_mean_pred) / dist_mean_st,
    global_prob_of_the_starting = as.numeric(bf$distr[loc_i_starting,date_starting] / 52),
    elapsed_days = elapsed_days,
    elapsed_km = elapsed_km,
    null_ll = null_ll,
    ll = ll,
    effective_win_distance = effective_win_distance,
    energy_score_bf = energy_score_pred,
    energy_score_st = energy_score_st,
    energy_improvement = energy_score_st - energy_score_pred,
    pred_elapsed_dist_by_pred = pred_elapsed_dist_by_pred,
    pred_elapsed_dist_by_st = pred_elapsed_dist_by_st
    ))
}


#' Get interval based metrics, including distance metrics for all transition pairs
#'
#' @param birdflow_intervals A BirdFlowIntervals object
#' @param bf BF model
#'
#' @return mean metrics across transition pairs
#' @export
get_interval_based_metrics <- function(birdflow_intervals, bf){
  # weekly distributions directly from S&T
  st_dists <- get_distr(bf, which = "all", from_marginals = FALSE)

  # Great circle distances between cells
  gcd <- great_circle_distances(bf)

  # Calculate distance metric & ll
  dists <- sapply(split(birdflow_intervals$data, seq(nrow(birdflow_intervals$data))), get_interval_based_validation_one_transition_pair, bf, gcd, st_dists)
  dists <- t(dists)

  dists <- as.data.frame(dists)
  # dists$ELC_distance <- exp(dists$ll) * dists$win_distance
  # dists$ELC_prob <- exp(dists$ll) * dists$win_prob
  # dists$RELC_distance <- exp(dists$ll - dists$null_ll) * dists$win_distance
  # dists$RELC_prob <- exp(dists$ll - dists$null_ll) * dists$win_prob
  
  # integrated metric by time
  dists_agg <- dists |>
    dplyr::group_by(elapsed_days) |>
    dplyr::summarize(win_prob = mean(win_prob), 
                     global_prob_of_the_starting = mean(global_prob_of_the_starting),
              win_distance = mean(win_distance), 
              win_distance_fraction = mean(win_distance_fraction),
              .groups = "drop")
  dists_agg <- dists_agg[order(dists_agg$elapsed_days), ]
  dists_agg$prob_weight <- dists_agg$global_prob_of_the_starting / sum(dists_agg$global_prob_of_the_starting)
  dx <- diff(dists_agg$elapsed_days) # Trapezoidal Rule
  area_win_prob_by_time <- sum(dx * (head(dists_agg$win_prob * dists_agg$prob_weight, -1) + tail(dists_agg$win_prob * dists_agg$prob_weight, -1)) / 2)
  area_win_distance_by_time <- sum(dx * (head(dists_agg$win_distance * dists_agg$prob_weight, -1) + tail(dists_agg$win_distance * dists_agg$prob_weight, -1)) / 2)
  area_win_distance_fraction_by_time <- sum(dx * (head(dists_agg$win_distance_fraction * dists_agg$prob_weight, -1) + tail(dists_agg$win_distance_fraction * dists_agg$prob_weight, -1)) / 2)
  
  # integrated metric by distance
  dists_agg <- dists |>
    dplyr::group_by(elapsed_km) |>
    dplyr::summarize(win_prob = mean(win_prob), 
                     global_prob_of_the_starting = mean(global_prob_of_the_starting),
              win_distance = mean(win_distance), 
              win_distance_fraction = mean(win_distance_fraction),
              .groups = "drop")
  dists_agg <- dists_agg[order(dists_agg$elapsed_km), ]
  dists_agg$prob_weight <- dists_agg$global_prob_of_the_starting / sum(dists_agg$global_prob_of_the_starting)
  dx <- diff(dists_agg$elapsed_km) # Trapezoidal Rule
  area_win_prob_by_distance <- sum(dx * (head(dists_agg$win_prob * dists_agg$prob_weight, -1) + tail(dists_agg$win_prob * dists_agg$prob_weight, -1)) / 2)
  area_win_distance_by_distance <- sum(dx * (head(dists_agg$win_distance * dists_agg$prob_weight, -1) + tail(dists_agg$win_distance * dists_agg$prob_weight, -1)) / 2)
  area_win_distance_fraction_by_distance <- sum(dx * (head(dists_agg$win_distance_fraction * dists_agg$prob_weight, -1) + tail(dists_agg$win_distance_fraction * dists_agg$prob_weight, -1)) / 2)
  
  n_intervals <- nrow(birdflow_intervals$data)
  
  output <- colMeans(dists)
  names(output) <- paste0('mean_',names(output))
  
  output <- c(output, 
              c(
                weighted_mean_win_prob=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$win_prob),
                weighted_mean_win_distance=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$win_distance),
                weighted_mean_null_ll=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$null_ll),
                weighted_mean_ll=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$ll),
                
                # weighted_mean_ELC_distance=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$ELC_distance),
                # weighted_mean_ELC_prob=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$ELC_prob),
                # weighted_mean_RELC_distance=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$RELC_distance),
                # weighted_mean_RELC_prob=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$RELC_prob),

                weighted_mean_effective_win_distance=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$effective_win_distance),
                weighted_energy_improvement=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$energy_improvement),
                weighted_energy_improvement_days_integral=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$energy_improvement * dists$elapsed_days),
                weighted_energy_improvement_kms_integral=sum((dists$global_prob_of_the_starting / sum(dists$global_prob_of_the_starting)) * dists$energy_improvement * dists$elapsed_km),
                
                area_win_prob_by_time=area_win_prob_by_time,
                area_win_distance_by_time=area_win_distance_by_time,
                area_win_distance_fraction_by_time=area_win_distance_fraction_by_time,
                area_win_prob_by_distance=area_win_prob_by_distance,
                area_win_distance_by_distance=area_win_distance_by_distance,
                area_win_distance_fraction_by_distance=area_win_distance_fraction_by_distance,
                n_intervals=n_intervals
                  )
              )
  
  
  return(list(output, dists))
}

