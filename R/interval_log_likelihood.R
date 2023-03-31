#' calculate log likelihoods of observed bird movements
#'
#' This function calculates the log likelihoods of inferred bird movement
#' based on two observation points (in time and space). The second point must
#' have a different timestep (week) than the first, but the location can remain
#' the same.
#'
#' @details
#' The core of this function is calling `predict()` on a distribution that has
#' the starting location hot (value of 1) and all other locations zero and then
#' extracting the probability of the ending week and location. The
#' log of this probability is returned in the `log_likelihood` column. The null
#' model assumes that the ebird S&T distribution and thus the `null_ll` column
#' contains the log of the probability density from the S&T distribution
#' at the ending week and location.
#'
#' The observations and intervals are separated into two tables to allow
#' flexibility in assigning and evaluating intervals.  With tracking data in
#' which the frequency of observations is much greater than the weekly S&T data
#' there are a lot of choices to be made and this function leaves those
#' decisions to the user.
#'
#' @param intervals  a data.frame that describes intervals (movements or
#' stationary periods) for which log likelihood will be calculated by
#' referencing the `id` column in `observations`.
#'  \describe{
#'  \item{`from`}{ observation id of the starting location and date}
#'  \item{ `to` }{ observation id of the ending location and date}
#'  \item{ ...  }{ any additional columns will be included in the returned
#'  object but not used by this function. It probably should include an
#'  interval ID.}
#'  }
#' @param observations a data.frame describing observations of birds each row
#' should be an individual bird, at a location, and date.
#'  \describe{
#'  \item{`id`}{ Unique observation identifier }
#'  \item{`lon` , `lat`}{  longitude and latitude of observation in WGS84
#'  (EPSG:4326) }
#' \item{ `date`}{ date associated with observation. See [lookup_timestep()] for
#'  valid formats.}
#' \item{ ... }{ Other columns allowed, but will be ignored. }
#'  }
#' @param bf a BirdFlow object
#' @param one_at_a_time Mainly here for debugging. If FALSE, the default, then
#' all intervals that start at the same timestep are processed together,
#' otherwise each interval is processed separately. Results should be identical,
#' TRUE uses less memory but is slower.
#'
#' @return The intervals table is returned  along with new columns:
#'  \item{log_likelihood }{ The model derived log likelihood of the interval }
#'  \item{ null_ll }{ the log likelihood of the interval based on a null model
#'  that assumes the eBird S&T distribution for the species at the end point }
#'  \item{ lag }{ the number of timesteps (likely weeks) between the start and
#'  end of the interval }
#'  \item{  exclude }{ TRUE if the log likelihood couldn't be calculated for
#'  the interval, in which case there should also be a TRUE in one of the
#'   remaining columns indicating why. }
#'  \item{ not_active }{ If TRUE the start or end point is not within the model
#'   mask }
#'  \item{ dynamic_mask }{ If TRUE ebirds S&T has assigned zero probability to
#'  the the start or end point for the associated date and therefore it is
#'  excluded by the dynamic mask or state based sparsification }
#'  \item{ sparse }{ TRUE if the model assigned zero probability to the interval
#'  and it wasn't due to any of the other reasons. This is likely due to
#'  sparsification eliminating all possible routes between the start and
#'  end point.}
#'  \item{ same_timestep }{ TRUE if the start and end timesteps are the same, a
#'   lag of zero }
#'  \item{ bad_date }{ TRUE if the date couldn't be parsed, or if `bf` is a
#'   partial model and the date falls in the uncovered portion of the year }
#'
#'  The returned table rows will have a 1:1 correspondence with the input
#'  `intervals` table.
#' @export
#'
#' @examples
#' \dontrun{
#' bf <- BirdFlowModels::rewbla
#' observations <- BirdFlowModels::rewbla_observations
#' intervals <- BirdFlowModels::rewbla_intervals
#' ll <- interval_log_likelihood(bf, observations, intervals)
#' }
interval_log_likelihood <- function(intervals, observations, bf,
                                   one_at_a_time = FALSE) {

  verbose <- birdflow_options("verbose")

  stopifnot("date" %in% names(observations),
            "lon" %in% names(observations),
            "lat" %in% names(observations),
            "id" %in% names(observations))

  stopifnot("from" %in% names(intervals),
            "to" %in% names(intervals))

  retained_new_columns <-
    c("log_likelihood", "null_ll", "lag", "exclude", "not_active",
      "dynamic_mask", "sparse", "same_timestep", "bad_date")

  if (any(retained_new_columns %in% names(intervals))) {
    conflicts <- intersect(retained_new_columns, names(intervals))
    warning("These columns will be replaced in the output: '",
            paste0(conflicts, collapse = "', '"), "'", sep = "")
    intervals <- intervals[, !names(intervals) %in% conflicts, drop = FALSE]
  }

  obs <- observations
  intv <- intervals

  if (!all(intv$from %in% obs$id))
    stop("Not all `from` values are in the id column of observations")
  if (!all(intv$to %in% obs$id))
    stop("Not all `to` values are in the id column of observations")

  # Set distribution timestep labeling to "t1" etc,
  original_time_format <- birdflow_options("time_format")
  on.exit(birdflow_options(time_format = original_time_format))
  birdflow_options(time_format = "timestep")

  # Convert observation lat, lon to to x,y and state index (i)
  obs_sf <- sf::st_as_sf(obs, coords = c("lon", "lat"))
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
  mv <- match(intv$from, obs$id)
  intv$i1 <- obs$i[mv]
  intv$t1 <- obs$ts[mv]
  mv <- match(intv$to, obs$id)
  intv$i2 <- obs$i[mv]
  intv$t2 <- obs$ts[mv]

  # Determine the lag  (weeks between observations)
  cyclical <- n_transitions(bf) == n_timesteps(bf)
  same_year <-   intv$t1 <= intv$t2
  intv$lag[same_year] <- intv$t2[same_year] - intv$t1[same_year]
  if (cyclical) {
    intv$lag[!same_year] <-
      intv$t2[!same_year] + n_timesteps(bf) - intv$t1[!same_year]
  } else {
    intv$lag[!same_year] <- NA
  }

  # Add logical columns for problems and exclusions
  intv$bad_date <- intv$same_timestep <- intv$not_active <-
    intv$dynamic_mask <- intv$sparse <-  intv$exclude  <- FALSE

  # Exclude NA lags (these are caused by date issues)
  sv <- is.na(intv$lag)
  intv$exclude[sv]  <- TRUE
  intv$bad_date[sv] <- TRUE

  # Exclude intervals where the starting or ending location aren't within
  # active cells.
  sv <- is.na(intv$i1) | is.na(intv$i2)
  intv$not_active[sv] <-  TRUE
  intv$exclude[sv] <- TRUE

  # Exclude locations that are not modeled at the given timestep
  valid <- is_location_valid(i = intv$i1, timestep = intv$t1, bf = bf) &
    is_location_valid(i = intv$i2, timestep = intv$t2, bf = bf)
  # Valid will also capture problems above so only capturing new problems here
  sv <- !valid & !intv$not_active & !intv$bad_date
  intv$dynamic_mask[sv] <- TRUE
  intv$exclude[!valid] <- TRUE

  # Exclude intervals that start and end in the same timestep
  sv <- intv$t1 == intv$t2
  intv$exclude[sv] <- TRUE
  intv$same_timestep[sv] <- TRUE

  # Calculate log likelihood

  # The one at a time implementation is more straight forward but takes
  # about 4 times as long.
  if (one_at_a_time) {
    rows <- which(!intv$exclude)
    if (verbose)
      pb <- progress::progress_bar$new(
        total = length(rows),
        format = " Calculating log likelihoods [:bar] :percent eta: :eta")
    likelihood <- rep(NA_real_, nrow(intv))
    null_likelihood <- rep(NA_real_, nrow(intv))

    for (j in seq_along(rows)) {
      r <- rows[j] # row in intv table
      t1 <- intv$t1[r]
      t2 <- intv$t2[r]
      i1 <- intv$i1[r]
      i2 <- intv$i2[r]
      d1 <- rep(0, n_active(bf))
      d1[i1] <- 1
      pred_d <- predict(bf, distr = d1, start = t1, end = t2,
                        direction = "forward")
      likelihood[r] <- pred_d[i2, ncol(pred_d)]

      # Null likelihood based on random sample from full distribution
      full_d <- get_distr(bf, t2)
      null_likelihood[r] <- full_d[i2]
      if (verbose)
        pb$tick()
    }

  } else {

    # Batch implementation

    # Process in batches based on shared starting timestep. The predict()
    # function can take multiple distributions in a matrix internally using
    # matrix multiplication against the transition matrix for each timestep to
    # project all of them at once, it then returns a three dimensional array
    # with dimensions location, distribution, and time from which all the ending
    # probabilities can be extracted at once for the batch.
    # One trade off is it uses more memory, another is that all the starting
    # locations for a given timestep are projected forward
    # to the latest ending timestep so there's a bunch of unnecessary
    # calculations done for the short intervals.
    # Empirically it seems to take about 1/4 the time of the other approach.

    starting_timesteps <- sort(unique(intv$t1[!intv$exclude]))

    if (verbose) {
      pb <- progress::progress_bar$new(
        total = sum(!intv$exclude),
        format = " Calculating log likelihoods [:bar] :percent eta: :eta")
    }

    likelihood <- rep(NA_real_, nrow(intv))
    null_likelihood <- rep(NA_real_, nrow(intv))

    for (j in seq_along(starting_timesteps)) {
      t1 <- starting_timesteps[j]  # starting timestep (common to all)
      sv <- intv$t1 == t1 & !intv$exclude
      i1s <- intv$i1[sv] # starting locations
      i2s <- intv$i2[sv] # ending locations
      t2s <- intv$t2[sv] # ending timesteps

      # Figure out last step to project forward to accounting for cyclical year
      if (cyclical && any(t2s < t1)) {
        endt <- max(t2s[t2s < t1])
      } else {
        endt <- max(t2s)
      }

      # Create starting distributions (1 distribution per column)
      d1 <- matrix(0, nrow = n_active(bf), ncol = sum(sv))
      d1[cbind(i1s, seq_along(i1s))] <- 1

      # Project together to last timestep
      ds <- predict(bf, distr = d1, start = t1, end = endt)

      # Pull out probability of ending location and time
      time_index <- match(paste0("t", t2s), dimnames(ds)[[3]])
      sel <- cbind(i2s, seq_along(i1s), time_index)
      likelihood[sv] <-  ds[sel]

      # Retrieve values for ending location from the distributions for the
      # species (eBird S&T distribution) at the ending timesteps
      full_distr <- get_distr(bf, which = t2s)
      if (!is.matrix(full_distr))
        full_distr <- matrix(full_distr, ncol = 1)
      null_likelihood[sv] <- full_distr[cbind(i2s, seq_along(i2s))]

      if (verbose)
        pb$tick(len = sum(sv))
    }
  }

  intv$log_likelihood <- log(likelihood)
  intv$null_ll  <- log(null_likelihood)

  # Given that we've already excluded intervals that start and
  # end at dropped states, all of these zero probability intervals
  # should be because sparsification eliminated all the routes that connect
  # the pair of observations.
  sv <- likelihood %in%  0
  intv$sparse[sv] <- TRUE
  intv$exclude[sv] <- TRUE
  intv$log_likelihood[sv] <- NA

  # Double check that rows still correspond
  stopifnot(all(intv$from == intervals$from, intv$to == intervals$to))

  return(cbind(intervals, intv[, retained_new_columns]))

}
