#' Calculate the predictive distance metric:
#' What is the probability that BirdFlow model prediction beats the naive S&T probability distribution-based prediction?

# util function 1
check_b_e_valid_cells <- function(b_e, bf){
  for (row in 1:2){
    res <- TRUE
    xy <- latlon_to_xy(lat = b_e$lat[row], lon = b_e$lon[row], bf)
    i <- xy_to_i(xy$x, xy$y, bf)
    res <- is_location_valid(bf = bf, i = i, date = b_e$date[row])
    if (!res) return(FALSE)
  }
  return(TRUE)
}

# util function 2
predict_encounter_distribution <- function(b_e, bf, gcd, st_dists){
  if (!check_b_e_valid_cells(b_e, bf)) return(c(pred = NA_real_, st = NA_real_, YK_win_prob = NA_real_, 
                                                global_prob_of_the_banding_starting = NA_real_, elapsed = NA_integer_, distance_winning_fraction = NA_real_))
  
  # latlong data for banding and encounter location
  point_df_initial <- data.frame(x = b_e$lon[1], y = b_e$lat[1])
  point_df_final   <- data.frame(x = b_e$lon[2], y = b_e$lat[2])
  # birdflow one-hot distributions for banding and encounter locations
  d_initial <- as_distr(x = point_df_initial, bf = bf, crs = 'EPSG:4326')
  d_final <- as_distr(x = point_df_final, bf = bf, crs = 'EPSG:4326')
  # get s&t distribution for final timestep
  final_timestep <- lookup_timestep(b_e$date[2], bf)
  final_st_distr <- st_dists[,final_timestep]
  # birdflow cell index for encounter location
  i_final <- which(d_final == 1)
  # birdflow predictions from banding one-hot, for encounter date
  preds <- predict(bf, d_initial, start = b_e$date[1], end = b_e$date[2])
  preds_final <- preds[,ncol(preds),drop = FALSE]
  preds_final <- as.vector(preds_final)
  # subset great circle distances for cell of actual encounter location
  gcd_final <- gcd[,i_final]
  # weighted average distance from predicted encounter distribution to actual encounter location
  
  # Dave's distance metric
  dist_mean_pred <- sum(preds_final * gcd_final)
  dist_mean_st   <- sum(final_st_distr * gcd_final)
  
  ## YK's function
  # randomly sample the arrival point, and calculate prob
  sampled_pred <- sample(1:length(preds_final), size=1000, prob=preds_final, replace=TRUE)
  sampled_final_st_distr <- sample(1:length(final_st_distr), size=1000, prob=final_st_distr, replace=TRUE)
  YK_win_prob <- mean(gcd_final[sampled_pred] < gcd_final[sampled_final_st_distr])
  
  # get location index of banding starting point
  xy_starting <- latlon_to_xy(b_e$lat[1], b_e$lon[1], bf)
  loc_i_starting <- xy_to_i(xy_starting$x[1], xy_starting$y[1], bf)
  date_starting <- lookup_timestep(b_e$date[1], bf)
  
  # return
  return(c(pred = dist_mean_pred, st = dist_mean_st, 
    YK_win_prob = YK_win_prob, 
    global_prob_of_the_banding_starting = as.numeric(bf$distr[loc_i_starting,date_starting] / 52),
    elapsed = as.integer(max(b_e$days, na.rm = TRUE)),
    distance_winning_fraction = (dist_mean_st - dist_mean_pred) / dist_mean_st
    ))
  
}


get_distance_metric <- function(intervals, observations, bf){
  ## Must have 2 banding data
  if (length(unique(observations$BAND_TRACK)) < 2) {
    return(NA)
  }
  b_e <- track_info$obs_df %>% group_by(BAND_TRACK) %>% filter(max(distance, na.rm = TRUE) > 100) %>% ungroup

  # weekly distributions directly from S&T
  st_dists <- get_distr(bf, which = "all", from_marginals = FALSE)

  # Great circle distances between cells
  gcd <- great_circle_distances(bf)

  # Calculate distance metric
  track_info$obs_df <- track_info$obs_df %>% group_by(BAND_TRACK) %>% filter(max(distance, na.rm = TRUE) > 100) %>% ungroup
  band_tracks <- split(track_info$obs_df, track_info$obs_df$BAND_TRACK)

  band_tracks <- sample(band_tracks, min(1000, length(band_tracks)))
  dists <- sapply(band_tracks, predict_encounter_distribution, bf, gcd, st_dists)
  dists <- t(dists)

  dists <- as.data.frame(dists)
  dists$win <- dists$st - dists$pred
  
  dists_dropna <- na.omit(dists)
  wining_proba <- sum(dists_dropna$global_prob_of_the_banding_starting / sum(dists_dropna$global_prob_of_the_banding_starting) *dists_dropna$YK_win_prob)
  
  dists$wining_proba_by_distance_metric <- mean(na.omit(dists$win)>0)
  dists$wining_proba_by_conditional_probability_YK <- wining_proba

  return(mean(na.omit(dists$wining_proba_by_distance_metric)))
}

