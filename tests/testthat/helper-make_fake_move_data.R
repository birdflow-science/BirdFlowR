make_fake_move_data <- function(bf){
  # This makes a dataframe with bird_id, track_id, x, y, date
  # Dates are monotonic withing each combination of
  #  bird_id, track_id combination. There are several dates per week
  # x, and y are random valid locations for the associated date.
  #  There is no linearity to the tracks.
  # Initially written to test snap_to_birdflow()

  ###  Make test data  ###

  # 2 birds, 1 track for the first, 2 tracks for second.
  # dates of last track different from first two
  n1 <- 20
  d <- data.frame(bird_id = rep(1:2, each = n1),
                  track_id = rep(1, n1*2),
                  x = rep(NA_real_, n1*2),
                  y = rep(NA_real_, n1*2),
                  date = rep(seq(lubridate::ymd('2022-02-15'),
                                 lubridate::ymd('2022-04-15'), length.out = n1), 2))
  n2 <- 25
  d <- rbind(d, data.frame(bird_id = rep(2, n2),
                           track_id = rep(2, n2),
                           x = rep(NA_real_, n2),
                           y = rep(NA_real_, n2),
                           date = seq(
                             lubridate::ymd('2022-02-01'),
                             lubridate::ymd('2022-04-30'), length.out = n2)))

  # Generate x,y by randomly selecting non-dynamically masked locations
  # There's no general motion direction here - except perhaps becuase the
  # non-masked cells are shifting during migration
  d$timestep <- lookup_timestep(d$date, bf)
  dm <- get_dynamic_mask(bf)
  for(ts in unique(d$timestep)){
    sv <- d$timestep == ts
    n <- sum(sv)
    i <- sample(which(dm[ , ts]), n, replace = TRUE)
    d[sv, c("x", "y")] <- i_to_xy(i, bf)
  }
  d$timestep <- NULL

  # Add noise
  rad <- xres(bf)/2
  d$x <- d$x + runif(nrow(d), -rad, rad)
  d$y <- d$y + runif(nrow(d), -rad, rad)

  return(d)
}



