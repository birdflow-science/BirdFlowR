test_that("route_between() returns BirdFlowRoutes with correct dimensions", {
  bf <- BirdFlowModels::amewoo
  set.seed(1)

  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
  n <- 10
  start_ts <- lookup_timestep("2023-02-15", bf)
  end_ts   <- lookup_timestep("2023-05-01", bf)
  expected_steps <- length(lookup_timestep_sequence(bf, start = start_ts,
                                                    end = end_ts))
  rts <- route_between(bf, n = n,
                       x_coord = xy$x, y_coord = xy$y,
                       date    = c("2023-02-15", "2023-05-01"))

  expect_s3_class(rts, "BirdFlowRoutes")
  expect_equal(length(unique(rts$data$route_id)), n)
  expect_equal(length(unique(rts$data$timestep)), expected_steps)
  expect_equal(nrow(rts$data), n * expected_steps)
})


test_that("route_between() pins routes to hard observations", {
  bf <- BirdFlowModels::amewoo
  set.seed(2)

  # Two known locations as lat/lon
  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
  i_start <- xy_to_i(xy$x[1], xy$y[1], bf)
  i_end   <- xy_to_i(xy$x[2], xy$y[2], bf)

  rts <- route_between(bf, n = 20,
                       x_coord = xy$x, y_coord = xy$y,
                       date    = c("2023-02-15", "2023-05-01"))

  start_ts <- lookup_timestep("2023-02-15", bf)
  end_ts   <- lookup_timestep("2023-05-01", bf)

  i_at_start <- rts$data$i[rts$data$timestep == start_ts]
  i_at_end   <- rts$data$i[rts$data$timestep == end_ts]

  # All routes must pass through the exact observed cells
  expect_true(all(i_at_start == i_start))
  expect_true(all(i_at_end   == i_end))
})


test_that("route_between() works with soft potentials via column names", {
  bf <- BirdFlowModels::amewoo
  set.seed(3)

  ts <- lookup_timestep(c("2023-02-15", "2023-05-01"), bf)
  obs_matrix <- matrix(runif(n_active(bf) * 2), nrow = n_active(bf))
  colnames(obs_matrix) <- paste0("t", ts)

  expect_no_error(
    rts <- route_between(bf, n = 5, potentials = obs_matrix)
  )
  expect_s3_class(rts, "BirdFlowRoutes")
  expect_equal(length(unique(rts$data$route_id)), 5)
})


test_that("route_between() works with soft potentials via date argument", {
  bf <- BirdFlowModels::amewoo
  set.seed(4)

  obs_matrix <- matrix(runif(n_active(bf) * 2), nrow = n_active(bf))

  expect_no_error(
    rts <- route_between(bf, n = 5,
                         potentials = obs_matrix,
                         date = c("2023-02-15", "2023-05-01"))
  )
  expect_s3_class(rts, "BirdFlowRoutes")
})


test_that("route_between() errors on bad input", {
  bf <- BirdFlowModels::amewoo
  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)

  # Both hard and soft obs provided
  obs_matrix <- matrix(1, nrow = n_active(bf), ncol = 2)
  expect_error(
    route_between(bf, n = 5, x_coord = xy$x, y_coord = xy$y,
                  date = c("2023-02-15", "2023-05-01"),
                  potentials = obs_matrix),
    regexp = "not both"
  )

  # Neither provided
  expect_error(route_between(bf, n = 5), regexp = "Must provide")

  # Hard obs missing date
  expect_error(
    route_between(bf, n = 5, x_coord = xy$x, y_coord = xy$y),
    regexp = "date is required"
  )

  # Mismatched lengths
  expect_error(
    route_between(bf, n = 5, x_coord = xy$x, y_coord = xy$y,
                  date = "2023-02-15"),
    regexp = "same length"
  )

  # potentials with wrong number of rows
  expect_error(
    route_between(bf, n = 5, potentials = matrix(1, nrow = 10, ncol = 2),
                  date = c("2023-02-15", "2023-05-01")),
    regexp = "n_active"
  )

  # potentials with no timestep info
  expect_error(
    route_between(bf, n = 5, potentials = matrix(1, nrow = n_active(bf), ncol = 2)),
    regexp = "column names"
  )
})


test_that("route_between() marginal matches predict() with binary-mask start potential", {
  # Statistical correctness test:
  # A binary mask over the start distribution defines a constrained initial
  # distribution. route_between() should sample routes whose final positions
  # are distributed according to predict() propagated from that same initial
  # distribution. We compare empirical mean x and y to the theoretical values.

  bf <- BirdFlowModels::amewoo
  set.seed(42)

  start_ts <- 9   # early March
  end_ts   <- 20  # mid May

  # Build binary mask: cells covering >= 60% of the marginal probability mass
  marginal <- get_distr(bf, start_ts)
  sorted_probs <- sort(marginal, decreasing = TRUE)
  threshold <- sorted_probs[min(which(cumsum(sorted_probs) >= 0.6))]
  mask_start <- as.numeric(marginal >= threshold)

  # Uniform potential at end_ts (no constraint, just extends the time range)
  mask_end <- rep(1, n_active(bf))

  obs_matrix <- cbind(mask_start, mask_end)
  colnames(obs_matrix) <- paste0("t", c(start_ts, end_ts))

  # Sample routes
  n <- 2000
  rts <- route_between(bf, n = n, potentials = obs_matrix)

  # Empirical mean x and y at the end timestep
  end_rows <- rts$data[rts$data$timestep == end_ts, ]
  empirical_mean_x <- mean(end_rows$x)
  empirical_mean_y <- mean(end_rows$y)

  # Theoretical: propagate the normalized start mask forward with predict()
  distr_start <- mask_start / sum(mask_start)
  pred <- predict(bf, distr_start, start = start_ts, end = end_ts)
  end_distr <- pred[, ncol(pred)]
  end_distr <- end_distr / sum(end_distr)

  all_x <- i_to_x(seq_len(n_active(bf)), bf)
  all_y <- i_to_y(seq_len(n_active(bf)), bf)
  theoretical_mean_x <- sum(all_x * end_distr)
  theoretical_mean_y <- sum(all_y * end_distr)

  # With n=2000 and typical spatial spread ~300 km, SE ≈ 7 km.
  # Tolerance of 50 km is ~7 SEs, so this should pass with very high probability.
  tol <- 50000  # 50 km in metres (the model CRS is in metres)
  expect_equal(empirical_mean_x, theoretical_mean_x, tolerance = tol)
  expect_equal(empirical_mean_y, theoretical_mean_y, tolerance = tol)
})
