test_that("Routes() -> as_BirdFlowRoutes() -> as_BirdFlowIntervals() works", {
  set.seed(42)

  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- "amewoo"
  source1 <- "BirdFlow"

  expect_no_error(my_routes <- Routes(fake_routes,
    species = species1,
    source = source1
  ))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf))
  expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))
})



test_that("Test Interval sampling strategy", {
  set.seed(42)

  # Setup
  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- species(bf)
  source1 <- "Testing"

  expect_no_error(my_routes <- Routes(fake_routes,
    species = species1,
    source = source1
  ))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf))


  # Constraints
  min_day <- 7
  max_day <- 180
  min_km <- 200
  max_km <- 8000

  expect_no_error(
    my_intervals <- BirdFlowR::as_BirdFlowIntervals(my_bfroutes,
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
    my_intervals <- as_BirdFlowIntervals(my_bfroutes,
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
  set.seed(42)
  fake_routes <- make_fake_routes_one_point_per_route()
  bf <- BirdFlowModels::amewoo
  species1 <- "amewoo"
  source1 <- "BirdFlow"

  expect_no_error(my_routes <- Routes(fake_routes,
    species = species1,
    source = source1
  ))
  expect_no_error(my_bfroutes <-
    as_BirdFlowRoutes(my_routes, bf = bf, valid_only = TRUE))
  expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))
  expect_null(my_intervals)
})
