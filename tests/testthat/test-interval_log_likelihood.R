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


test_that(
  "interval_log_likelihood() sets right flags for problematic locations", {

  bf <- BirdFlowModels::rewbla
  intervals <- BirdFlowModels::rewbla_intervals[1:4, ]
  observations <- BirdFlowModels::rewbla_observations

  # I'm going to alter locations in observations such that the first four
  # intervals correspond with (a first observation) that is:
  # 1. valid location  ->  exclude = FALSE
  # (2 - 4 all should have exclude = TRUE)
  # 2. location in an active cell where distr is zero: "dynamic_mask" == TRUE
  # 3. in extent but not an active cell:  "not active" == TRUE
  # 4. not in extent:  "not active" == TRUE

  # 1. Valid.  Leave as is.

  # 2. dynamically masked
  r = 2
  obs_id <- intervals$from[r]
  obs_row <- which(observations$id == obs_id)
  t = lookup_timestep(observations$date[obs_row], bf)
  d <- get_distr(bf, t)
  i <- which(d == 0)[1]
  xy <- i_to_xy(i, bf) |> as.data.frame()
  pt <- sf::st_as_sf(xy, coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform( crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pt) <- c("lon", "lat")
  observations$lon[obs_row] <- pt$lon
  observations$lat[obs_row] <- pt$lat

  # 3. Not active (but in extent)
  r = 3
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
    sf::st_transform( crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pt) <- c("lon", "lat")
  observations$lon[obs_row] <- pt$lon
  observations$lat[obs_row] <- pt$lat

  # 4. Not in extent
  r = 4
  obs_id <- intervals$from[r]
  obs_row <- which(observations$id == obs_id)
  x <- xmax(bf) + 1000
  y <- ymax(bf) + 1000
  pt <- data.frame(x = x, y = y) |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crs(bf))) |>
    sf::st_transform( crs = "EPSG:4326") |>
    sf::st_coordinates() |>
    as.data.frame()
  names(pt) <- c("lon", "lat")
  observations$lon[obs_row] <- pt$lon
  observations$lat[obs_row] <- pt$lat

  expect_no_error( a <- interval_log_likelihood(intervals, observations, bf) )

  expect_equal(a$exclude, c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(a$not_active, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(a$dynamic_mask, c(FALSE, TRUE, FALSE, FALSE))


})


test_that( "interval_log_likelihood() throws warning if overwriting columns", {

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






