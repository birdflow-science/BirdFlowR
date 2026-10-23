test_that(paste0("interval_log_likelihood produces identical results with ",
                 "one_at_a_time = TRUE"), {
  skip_if_not_installed("BirdFlowExamples")
  intervals <- BirdFlowExamples::rewbla_intervals
  observations <- BirdFlowExamples::rewbla_observations
  intervals <- intervals[1:60, ]
  bf <- BirdFlowExamples::rewbla
  expect_warning(a <- interval_log_likelihood(intervals, observations, bf))
  expect_warning(
    b <- interval_log_likelihood(intervals, observations, bf,
                                 one_at_a_time = TRUE)
  )
  expect_equal(a, b)

  ll <- a[1:4, c("log_likelihood", "null_ll", "lag")]
  rownames(ll) <- NULL
  expect_snapshot(ll)
})


test_that("interval_log_likelihood returns expected values", {
  skip_if_not_installed("BirdFlowExamples")

  bf  <- BirdFlowExamples::rewbla
  nsteps <- 3

  # Use route() to make a fake observation table
  set.seed(0)
  start <- 6
  end <- start + nsteps
  i <- get_distr(bf, start) |> sample_distr() |> as.logical() |> which()
  observations <- route(bf, x_coord = i_to_x(i, bf), y_coord = i_to_y(i, bf),
              start = start, end = end)$data
  observations$id <- seq_len(nrow(observations))
  sf_obs <- sf::st_as_sf(observations, coords = c("x", "y"), crs = crs(bf))
  wgs <- sf::st_transform(sf_obs, sf::st_crs("EPSG:4326")) |>
    sf::st_coordinates()
  wgs <- wgs[, 1:2]
  colnames(wgs) <- c("lon", "lat")

  observations <- cbind(observations, wgs)
  observations$i <- xy_to_i(x = observations$x, y = observations$y, bf)
  intervals <- data.frame(from = 1:(nrow(observations) - 1),
                          to = 2:nrow(observations))

  # Calculate log likelihood for observation table
  expect_warning(ll <- interval_log_likelihood(intervals, observations, bf))

  # Pull single step log likelihood directly from the transition matrices.
  # Transition matrices are indexed to each timestep's dynamic mask subset,
  # not to the full set of active cells, so state indices (i) must be
  # converted to dynamic-mask indices (dmi) before indexing into them.
  expected_ll <- rep(NA_real_, nsteps)
  for (j in 1:nsteps) {
    t1 <- start + j - 1
    t2 <- start + j
    t_id <- lookup_transitions(bf, start = t1, end = t2, direction = "forward")
    t <- get_transition(bf, t_id)
    i1 <- i_to_dmi(observations$i[j], t1, bf)  # start location index
    i2 <- i_to_dmi(observations$i[j + 1], t2, bf)  # end location index
    expected_ll[j] <- log(t[i2, i1])
  }

  expect_equal(ll$log_likelihood, expected_ll)

})


test_that(
  "interval_log_likelihood() sets right flags for problematic locations", {
  skip_if_not_installed("BirdFlowExamples")

  bf <- BirdFlowExamples::rewbla
  intervals <- BirdFlowExamples::rewbla_intervals[1:4, ]
  observations <- BirdFlowExamples::rewbla_observations

  # I'm going to alter locations in observations such that the first four
  # intervals correspond with (a first observation) that is:
  # 1. valid location  ->  exclude = FALSE
  # (2 - 4 all should have exclude = TRUE)
  # 2. location in an active cell where distr is zero: "dynamic_mask" == TRUE
  # 3. in extent but not an active cell:  "not active" == TRUE
  # 4. not in extent:  "not active" == TRUE

  # 1. Valid.  Leave as is.

  # 2. dynamically masked
  r <- 2
  obs_id <- intervals$from[r]
  obs_row <- which(observations$id == obs_id)
  t <- lookup_timestep(observations$date[obs_row], bf)
  d <- get_distr(bf, t)
  i <- which(d == 0)[1]
  xy <- i_to_xy(i, bf) |> as.data.frame()
  pt <- sf::st_as_sf(xy, coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pt) <- c("lon", "lat")
  observations$lon[obs_row] <- pt$lon
  observations$lat[obs_row] <- pt$lat

  # 3. Not active (but in extent)
  r <- 3
  obs_id <- intervals$from[r]
  obs_row <- which(observations$id == obs_id)
  mask <- bf$geom$mask
  cell <- which(!mask)[1]
  r <- row(mask)[cell]
  c <- col(mask)[cell]
  x <- row_to_y(r, bf)
  y <- col_to_x(c, bf)
  pt <- data.frame(x = x, y = y) |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pt) <- c("lon", "lat")
  observations$lon[obs_row] <- pt$lon
  observations$lat[obs_row] <- pt$lat

  # 4. Not in extent
  r <- 4
  obs_id <- intervals$from[r]
  obs_row <- which(observations$id == obs_id)
  x <- xmax(bf) + 1000
  y <- ymax(bf) + 1000
  pt <- data.frame(x = x, y = y) |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pt) <- c("lon", "lat")
  observations$lon[obs_row] <- pt$lon
  observations$lat[obs_row] <- pt$lat

  expect_warning(a <- interval_log_likelihood(intervals, observations, bf))

  expect_equal(a$exclude, c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(a$not_active, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(a$dynamic_mask, c(FALSE, TRUE, FALSE, FALSE))


})


test_that("interval_log_likelihood() throws warning if overwriting columns", {
  skip_if_not_installed("BirdFlowExamples")

  # Also test that it works fine with verbose = FALSE
  # Temporarily suppress BirdFlowR chatter
  local_quiet()

  bf <- BirdFlowExamples::rewbla
  intervals <- BirdFlowExamples::rewbla_intervals[1:10, ]
  observations <- BirdFlowExamples::rewbla_observations

  expect_warning(a <- interval_log_likelihood(intervals, observations, bf))
  expect_warning(
    expect_warning(b <- interval_log_likelihood(a, observations, bf),
                   "These columns will be replaced in the output:"),
    "interval_log_likelihood\\(\\) is deprecated"
  )

  expect_equal(a, b)

})


test_that("Interval log likelihood handles empty input gracefully", {
  skip_if_not_installed("BirdFlowExamples")
  #  Checks issue 95  https://github.com/birdflow-science/BirdFlowR/issues/95
  bf <- BirdFlowExamples::rewbla
  intervals <- BirdFlowExamples::rewbla_intervals[1:10, ]
  intervals_empty <- intervals[rep(FALSE, nrow(intervals)), , drop = FALSE]
  observations <- BirdFlowExamples::rewbla_observations

  expect_warning(
    ll <- interval_log_likelihood(intervals_empty, observations, bf)
  )
  expect_equal(nrow(ll), 0)

  expect_warning(ll2 <- interval_log_likelihood(intervals, observations, bf))

  # Check that column names and classes are identical with or without data
  expect_equal(colnames(ll), colnames(ll2))
  expect_equal(sapply(ll, class), sapply(ll2, class))

})
