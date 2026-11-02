test_that("as_routes() -> as_birdflow_routes() -> as_birdflow_intervals() works", {
  skip_if_not_installed("BirdFlowExamples")
  set.seed(42)

  fake_routes <- make_fake_routes()
  bf <- BirdFlowExamples::amewoo
  species1 <- bf$species
  source1 <- "BirdFlow"

  expect_no_error(my_routes <- as_routes(fake_routes,
    species = species1,
    source = source1
  ))
  expect_no_error(my_bfroutes <- as_birdflow_routes(my_routes, bf = bf))
  expect_no_error(my_intervals <- as_birdflow_intervals(my_bfroutes))
})



test_that("Test Interval sampling strategy", {
  skip_if_not_installed("BirdFlowExamples")
  set.seed(42)

  # Setup
  fake_routes <- make_fake_routes()
  bf <- BirdFlowExamples::amewoo
  species1 <- bf$species
  source1 <- "Testing"

  expect_no_error(my_routes <- as_routes(fake_routes,
    species = species1,
    source = source1
  ))
  expect_no_error(my_bfroutes <- as_birdflow_routes(my_routes, bf = bf))


  # Constraints
  min_day <- 7
  max_day <- 180
  min_km <- 200
  max_km <- 8000

  expect_no_error(
    my_intervals <- BirdFlowR::as_birdflow_intervals(my_bfroutes,
      max_n = 1000,
      min_day_interval = min_day,
      max_day_interval = max_day,
      min_km_interval = min_km,
      max_km_interval = max_km
    )
  )

  # Check that constraints are honored
  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1)^2)) / 1000
  lag <- with(my_intervals$data, date2 - date1) |> as.numeric(units = "days")
  expect_true(all(dist_km >= min_km))
  expect_true(all(dist_km <= max_km))
  expect_true(all(lag >= min_day))
  expect_true(all(lag <= max_day))

  # Number of routes (1)
  expect_no_error(
    my_intervals <- as_birdflow_intervals(my_bfroutes,
      max_n = 1,
      min_day_interval = min_day,
      max_day_interval = max_day,
      min_km_interval = min_km,
      max_km_interval = max_km
    )
  )
  expect_equal(nrow(my_intervals$data), 1)
  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1)^2)) / 1000
  lag <- with(my_intervals$data, date2 - date1) |> as.numeric(units = "days")
  expect_true(all(dist_km >= min_km))
  expect_true(all(dist_km <= max_km))
  expect_true(all(lag >= min_day))
  expect_true(all(lag <= max_day))
})


test_that("If no intervals can be sampled, return NULL", {
  skip_if_not_installed("BirdFlowExamples")
  set.seed(42)
  fake_routes <- make_fake_routes_one_point_per_route()
  bf <- BirdFlowExamples::amewoo
  species1 <- bf$species
  source1 <- "BirdFlow"

  expect_no_error(my_routes <- as_routes(fake_routes,
    species = species1,
    source = source1
  ))
  expect_no_error(my_bfroutes <-
    as_birdflow_routes(my_routes, bf = bf, valid_only = TRUE))
  expect_no_error(my_intervals <- as_birdflow_intervals(my_bfroutes))
  expect_null(my_intervals)
})


test_that("Routes with one valid pair are not silently dropped.  #254", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo
  set.seed(42)

  # Build routes from route() output (guaranteed to snap validly to the
  # model grid), keeping only two far-apart timesteps for four routes
  # (one valid interval pair each) and five timesteps for a fifth route
  # (multiple valid pairs).
  synthetic <- route(bf = bf, n = 5)$data
  pick <- function(route, timesteps) {
    route <- route[route$route_id == route$route_id[1], ]
    route <- route[!duplicated(route$timestep), ]
    route <- route[route$timestep %in% timesteps, ]
    route <- route[order(route$timestep), ]
    data.frame(
      route_id = route$route_id,
      date = route$date,
      lon = route$lon,
      lat = route$lat,
      route_type = "tracking"
    )
  }
  df <- rbind(
    pick(synthetic[synthetic$route_id == 1, ], c(1, 15)),
    pick(synthetic[synthetic$route_id == 2, ], c(1, 15)),
    pick(synthetic[synthetic$route_id == 3, ], c(1, 15)),
    pick(synthetic[synthetic$route_id == 4, ], c(1, 15)),
    pick(synthetic[synthetic$route_id == 5, ], c(1, 8, 15, 22, 29))
  )
  df$route_id <- as.character(df$route_id)

  rts <- as_routes(df, species = "amewoo") |> as_birdflow_routes(bf = bf)
  iv <- as_birdflow_intervals(rts, max_n = 7)

  routes_sampled <- iv$data$route_id |> unique() |> length()
  expect_equal(routes_sampled, 5)
})
