test_that("intererval_log_likelihood produces identical results with one_at_a_time = TRUE", {
  intervals <- BirdFlowModels::rewbla_intervals
  observations <- BirdFlowModels::rewbla_observations
  intervals <- intervals[1:60, ]
  bf <- BirdFlowModels::rewbla
  a <- interval_loglikelihood(bf, observations, intervals)
  b <- interval_loglikelihood(bf, observations, intervals, one_at_a_time = TRUE)
  expect_equal(a, b)

  ll <- a[1:4, c("log_likelihood", "null_ll", "lag")]
  rownames(ll) <- NULL
  expect_snapshot(ll)

})


test_that("interval_log_likelihood returns expected values", {

  bf  <- BirdFlowModels::rewbla

  # Use route() to make a fake observation table
  set.seed(0)
  start <- species_info(bf, "prebreeding_migration_start") |>
    lookup_timestep(bf) |> sum(1)
  end <- start + 3
  i <- get_distr(bf, start) |> sample_distr() |> as.logical() |> which()
  rt <- route(bf, x_coord = i_to_x(i, bf), y_coord = i_to_y(i, bf),  start = start, end = end)
  observations <- rt$points
  observations$id <- 1:nrow(observations)
  sf_obs <- sf::st_as_sf(observations, coords= c("x", "y"))
  sf::st_crs(sf_obs) <- crs(bf)
  wgs <- sf::st_transform(sf_obs, sf::st_crs("EPSG:4326")) |> sf::st_coordinates()
  colnames(wgs) <- c("lon", "lat")
  observations <- cbind(observations, wgs)
  observations$i <- xy_to_i(x = observations$x, y = observations$y, bf)
  intervals <- data.frame(from = 1:(nrow(observations) - 1),
                          to = 2:nrow(observations))

  # Calculate log likelihood for observation table
  ll <- interval_loglikelihood(bf, observations, intervals)

  # First log likelihood can be pulled directly from the transition matrix
  # from the 1st to 2nd modeled timesteps.
  t <- get_transition(bf, paste0("T_0", start, "-0", start+1))
  i2 <- observations$i[2]
  expect_equal(log(t[i2, i]), ll$log_likelihood[1])

})
