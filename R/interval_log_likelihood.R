if(FALSE){

  # Example data

  # Paths
  dir <- "../Models/band_example_for_get_loglikelihood/"
  species <- "gwfgoo"

  # BirdFlow
  hdf5 <- paste0(dir, species, "_trained.hdf5")
  bf <- import_birdflow(hdf5) |>
    sparsify(method = "state") |>
    build_transitions()

  # obseration data
  obs_path <-  paste0(dir, species, ".rds")
  obs <- as.data.frame(readRDS(obs_path))
  names(obs) <- tolower(names(obs))
  obs$observation_id <- 1:nrow(obs)
  sv <- obs$band != obs$original_band
  sv <- obs$other_bands != "" &  obs$original_band != obs$other_bands



  # Construct data frame of pairs of bands known to be the same
  sv <- obs$band != obs$original_band
  same_band <- data.frame(a = obs$band[sv], b =  obs$original_band[sv])

  sv <- obs$other_bands != "" &  obs$original_band != obs$other_bands
  same_band <- rbind(same_band,
                     data.frame(a = obs$original_band[sv], b = obs$other_bands[sv]))

  sv <- obs$other_bands != "" &  obs$band != obs$other_bands
  same_band <- rbind(same_band,
                     data.frame(a = obs$band[sv], b = obs$other_bands[sv]))
  same_band <- same_band[!duplicated(same_band), ]

  sv <- grepl(";.+$", same_band$a) | grepl(";.+$", same_band$b)
  if(any(sv))
    stop("Multiple bands stored in other_bands column. Need to handle differently.")
  same_band$a <- gsub(";$", "", same_band$a)
  same_band$b <- gsub(";$", "", same_band$b)

  sv <- grepl(";", same_band$a) | grepl(";", same_band$b)
  if(any(sv))
    stop("; in unexpected location")
  same_band <- apply(same_band, 1, sort , simplify = TRUE) |> t() |>
    as.data.frame()
  names(same_band) <- c("a", "b")
  same_band <- same_band[!duplicated(same_band), , drop = FALSE]
  same_band <- same_band[order(same_band$a), ]
  rownames(same_band) <- NULL
  same_band <- same_band[same_band$a != same_band$b ,  , drop = FALSE]

  # Same_band is sorted so that the band in column a is alphabetically before
  # the band in column b.
  # Here we iteratively replace any of the column b bands with the column a
  # equivallent until there's no bands left from any column b band.
  # I'm calling this the bird's ID.  It unites all observations of the bird
  # (under different bands), but may NOT be the original band ID.
  obs$bird_id <- obs$band
  i <- 1
  while(any(obs$bird_id %in% same_band$b)){
    sv <- obs$bird_id %in% same_band$b
    cat("Pass ", i, ":", sum(sv), "bands relabled\n")
    ok <- obs[!sv, , drop = FALSE]
    not_ok <- obs[sv, , drop = FALSE]
    mv <- match(not_ok$bird_id, same_band$b)
    # stopifnot(all(not_ok$bird_id == same_band$b[mv]))
    not_ok$bird_id <- same_band$a[mv]
    obs <- rbind(ok, not_ok)
    i <- i + 1
  }

  # Add unique observation ID (numeric)
  obs <- obs[order(obs$bird_id, obs$event_date), ]
  obs$id <- 1:nrow(obs)

  # save full table
  full_obs <- obs

  # and subset to what I care about
  obs <- obs[, c("id", "bird_id", "lat_dd", "lon_dd", "event_date")]
  obs <- dplyr::rename(obs, lat = lat_dd, lon = lon_dd, date = event_date)

  obs <-
    dplyr::group_by(obs, bird_id) %>%
    dplyr::mutate(obs_no = dplyr::row_number()) |>
    as.data.frame()

  # Make intervals

  #### For simplicities sake I'm just going to compare the first and second
  #### observation of each bird.  This isn't a good statistical choice but
  #### I just want example data.

  intervals <-
    obs[obs$obs_no %in% 1:2, c("bird_id", "id", "obs_no"), drop = FALSE] |>
    tidyr::pivot_wider(names_from = obs_no, values_from = id) |>
    dplyr::rename(from = `1`, to = `2`) |>
    as.data.frame()

  intervals$interval_id <- 1:nrow(intervals)

  # Now have test objects:
  head(intervals, 3)
  head(obs, 3)
  bf
  observations <- obs

}


interval_loglikelihood <- function(bf, observations, intervals) {


  stopifnot("date" %in% names(observations),
            "lon" %in% names(observations),
            "lat" %in% names(observations),
            "id" %in% names(observations))

  stopifnot("from" %in% names(intervals),
            "to" %in% names(intervals))

  obs <- observations
  intv <- intervals

  if(!all(intv$from %in% obs$id))
    stop("Not all from values in intervals are in the id column of observations")
  if(!all(intv$to %in% obs$id))
    stop("Not all from values in intervals are in the id column of observations")

  # Convert observation lat, lon to to x,y and state index (i)
  obs_sf <- sf::st_as_sf(obs, coords= c("lon", "lat"))
  sf::st_crs(obs_sf) <- sf::st_crs("EPSG:4326")
  obs_t <- sf::st_transform(obs_sf, sf::st_crs(crs(bf)))
  coords <- sf::st_coordinates(obs_t)
  colnames(coords) <- tolower(colnames(coords))
  stopifnot(colnames(coords) == c("x", "y"))
  obs <- obs[, !names(obs) %in% c("x", "y", "i", "timestep")]
  obs <- cbind(obs, coords)
  obs$i <- xy_to_i(obs$x, obs$y, bf)

  # Convert date to timestep
  obs$ts <- lookup_timestep(obs$date, bf)

  # Add starting and ending state index and dates to intervals table
  # 't' indicates timestep, 'i' indicates state index
  # suffix of '1' for starting and '2' for ending (date or timestep)

  ### Note date stuff is tricky.  We might want to move dates into intervals
  # before converting to timestep and verify that the ending dates is after
  # the starting date.   Weirdness can happen as is.
  #  For instance May 2020 to June 2021 will end up being a 1 month lag, not
  #  13 month.

  mv <- match(intv$from, obs$id )
  intv$i1 <- obs$i[mv]
  intv$t1 <- obs$ts[mv]
  mv <- match(intv$to, obs$id )
  intv$i2 <- obs$i[mv]
  intv$t2 <- obs$ts[mv]

  # Determine the lag  (weeks between observations)
  cyclical <- n_transitions(bf) == n_timesteps(bf)
  same_year<-   intv$t1 < intv$t2
  intv$lag[same_year] <- intv$t2[same_year] - intv$t1[same_year]
  if(cyclical){
    intv$lag[!same_year] <- intv$t2[!same_year] + n_timesteps(bf) - intv$t1[!same_year]
  } else {
    intv$lag[!same_year] <- NA
  }

  # Add exclude column and zero_prob
  # exclude covers anything we can't model (including dates out of the modeled
  # range if the model isn't for the full year).
  # zero_prob is for locations that the model indicates have zero probability
  # either because they aren't active cells or because they have zero
  # probability in the S&T data for that timestep. These you might want to
  # assign some very low predicted log likelihood to rather than drop from the
  # analysis.
  intv$exclude <- intv$zero_prob<- FALSE
  intv$exclude[is.na(intv$lag) | is.na(intv$i1) | is.na(intv$t2)] <- TRUE
  intv$zero_prob[is.na(intv$i1) | is.na(intv$t2)] <- TRUE

  head(intv)
  valid <- is_location_valid(i = intv$i1, timestep = intv$t1, bf = bf) &
    is_location_valid(i = intv$i2, timestep = intv$t2, bf = bf)
  intv$exclude[!valid] <- TRUE
  intv$zero_prob[!valid] <- TRUE

  intv$log_likelihood <- NA_real_

  starting_timesteps <- sort(unique(intv$t1))


  coast <- get_coastline(bf)

  one_at_a_time <- TRUE

  if(one_at_a_time){
    rows <- which(!intv$exclude)
    for(j in seq_along(rows)){
      r <- rows[j] # row in intv table
      t1 <- intv$t1[r]
      t2 <- intv$t2[r]
      i1 <- intv$i1[r]
      i2 <- intv$i2[r]
      d <- rep(0, n_active(bf))
      d[i1] <- 1
      ds <- predict(bf, distr = d, start = t1, end = t2)
      plot(rasterize_distr(ds[, 1], bf))
      plot(coast)



    }





  } else {

    # Here we are going to process all the locations that start at the timestep
    # in a batch, iterating through timesteps.


   for(j in seq_along(starting_timesteps)){
    t1 <- starting_timesteps[j]

    sv <- intv$t1 == t1 & !intv$exclude

    i1s <- intv$i1[sv]
    i2s <- intv$i2[sv]
    t2s <- intv$t2[sv]

    # Figure out last step to project forward to (accounting for cyclical year)

    if(any(t2s < t1)) {
      endt <- max(t2s[t2s < t1])
    } else {
      endt <- max(t2s)
    }
    # Create starting distributions (1 distribution per column)
    d1 <- matrix(0, nrow = n_active(bf), ncol = sum(sv))
    d1[cbind(i1s, 1:length(i1s))] <- 1

    ds <- predict(bf, distr = d1, start = t1, end = endt)


    stop("This isn't fully implemented.")

  }





}

