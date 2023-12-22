test_that("snap_to_birdflow works", {
  bf <- BirdFlowModels::amewoo

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

  ### Test ###

  # Test alignment using reprojection with no aggregation
  expect_no_error(
    a <- snap_to_birdflow(d, bf, x_col = "x", y_col = "y", crs = crs(bf),
                     id_cols = c("bird_id", "track_id"))
  )

  # Swap out lat and lon for x and y
  d2 <- cbind(d[, !names(d) %in% c("x", "y"), drop = FALSE],
              xy_to_latlon(d[, c("x","y"), drop = FALSE], bf =  bf))

  # Test align with default lat, lon, and no crs.
  expect_no_error(
  b <- snap_to_birdflow(d2, bf,
                   id_cols = c("bird_id", "track_id"))
  )

  expect_equal(a$error,  b$error)
  expect_equal(a$message, b$message)

  # Aggregate with median
  a1 <-  snap_to_birdflow(d2, bf,
                    id_cols = c("bird_id", "track_id"), aggregate = "median")

  # aggregate with mean
  a2 <-  snap_to_birdflow(d2, bf,
                    id_cols = c("bird_id", "track_id"), aggregate = "mean")

  # aggregate with midweek
  a3 <-  snap_to_birdflow(d2, bf,
                    id_cols = c("bird_id", "track_id"), aggregate = "midweek")


  consistent_cols <- c("bird_id", "track_id", "timestep", "n")
  expect_equal(a1[, consistent_cols], a2[ , consistent_cols])
  expect_equal(a1[, consistent_cols], a3[ , consistent_cols])


  # Expect median and mean to be identical if less than 3 observations
  sv <- d$n < 3
  expect_equal(a1[sv, c("x", "y")], a2[sv, c("x", "y")])

})
