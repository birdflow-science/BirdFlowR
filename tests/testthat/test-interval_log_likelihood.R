test_that("interval_log_likelihood produces identical results with one_at_a_time = TRUE", {
  intervals <- BirdFlowModels::rewbla_intervals
  observations <- BirdFlowModels::rewbla_observations
  intervals <- intervals[1:60, ]
  bf <- BirdFlowModels::rewbla
  a <- interval_log_likelihood(intervals, observations, bf)
  b <- interval_log_likelihood(intervals, observations, bf,
                               one_at_a_time = TRUE)
  expect_equal(a, b)

  ll <- a[1:4, c("log_likelihood", "null_ll", "lag")]
  rownames(ll) <- NULL
  expect_snapshot(ll)
})


test_that("interval_log_likelihood returns expected values", {

  bf  <- BirdFlowModels::rewbla
  nsteps <- 3

  # Use route() to make a fake observation table
  set.seed(0)
  start <- 6
  end <- start + nsteps
  i <- get_distr(bf, start) |> sample_distr() |> as.logical() |> which()
  rt <- route(bf, x_coord = i_to_x(i, bf), y_coord = i_to_y(i, bf),
              start = start, end = end)
  observations <- rt$points
  observations$id <- 1:nrow(observations)
  sf_obs <- sf::st_as_sf(observations, coords= c("x", "y"))
  sf::st_crs(sf_obs) <- crs(bf)
  wgs <- sf::st_transform(sf_obs, sf::st_crs("EPSG:4326")) |>
    sf::st_coordinates()
  colnames(wgs) <- c("lon", "lat")
  observations <- cbind(observations, wgs)
  observations$i <- xy_to_i(x = observations$x, y = observations$y, bf)
  intervals <- data.frame(from = 1:(nrow(observations) - 1),
                          to = 2:nrow(observations))

  # Calculate log likelihood for observation table
  ll <- interval_log_likelihood(intervals, observations, bf)

  # Pull single step log likelihood directly from the transition matrices
  expected_ll <- rep(NA_real_, nsteps)
  for (j in 1:nsteps) {
    t_id <- lookup_transitions(bf, start + j -1 ,  start + j, "forward")
    t <- get_transition(bf, t_id)
    i1 <- observations$i[j]  # start location index
    i2 <- observations$i[j + 1] # end location index
    expected_ll[j] <- log(t[i2, i1])
  }

  expect_equal(ll$log_likelihood, expected_ll)

})


test_that("interval_log_likelihood() throws warning if overwriting columns", {

  # Also test that it works fine with verbose = FALSE
  original_verbose <- birdflow_options("verbose")
  on.exit(birdflow_options(verbose = original_verbose))
  birdflow_options(verbose = FALSE)

  bf <- BirdFlowModels::rewbla
  intervals <- BirdFlowModels::rewbla_intervals[1:10, ]
  observations <- BirdFlowModels::rewbla_observations

  a <- interval_log_likelihood(intervals, observations, bf)
  expect_warning(b <- interval_log_likelihood(a, observations, bf),
                 "These columns will be replaced in the output:")

  expect_equal(a, b)

})


