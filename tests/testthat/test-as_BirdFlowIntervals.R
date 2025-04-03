test_that("Routes() -> as_BirdFlowRoutes() -> as_BirdFlowIntervals() works", {
  set.seed(42)

  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- 'amewoo'
  source1 <- "BirdFlow"

  expect_no_error(my_routes <- Routes(fake_routes,
                                      species = species1,
                                      source = source1))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
  expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))



  expect_snapshot(my_routes)
  expect_snapshot(my_bfroutes)
  expect_snapshot(my_intervals)

  # Here we will randomly select only one data point for route 001 at timestep 1,
  # so the snapshot is not necessarily the same.
  # But we are using seed, so it will always be the same.
})



test_that("Test Interval sampling strategy", {
  set.seed(42)

  # Setup
  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- species(bf)
  source1 <- "Testing"

  expect_no_error(my_routes <- Routes(fake_routes, species = species1,
                                      source = source1))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))


  # Constraints
  min_day <- 7
  max_day <- 180
  min_km <- 200
  max_km = 8000

  expect_no_error(
    my_intervals <- BirdFlowR::as_BirdFlowIntervals(my_bfroutes,
                                         max_n=1000,
                                         min_day_interval = min_day,
                                         max_day_interval = max_day,
                                         min_km_interval= min_km,
                                         max_km_interval = max_km))

  # This test is failing because the second half of the function doesn't
  # honor the valid pairs identified in the first half.
  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1) ^2)) / 1000
  lag <- with(my_intervals$data, date2 - date1) |> as.numeric(units = "days")
  expect_true(all(dist_km >= min_km))
  expect_true(all(dist_km <= max_km))
  expect_true(all(lag >= min_day))
  expect_true(all(lag <= max_day))

  # Number of routes (1)
  # Note this fails if run repeatedly but doesn't always fail because
  # random sampling sometimes selects valid intervals
  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=1,
                                         min_day_interval = min_day,
                                         max_day_interval = max_day,
                                         min_km_interval= min_km,
                                         max_km_interval = max_km))
  expect_equal(nrow(my_intervals$data), 1)
  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1) ^2)) / 1000
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

  expect_no_error(my_routes <- Routes(fake_routes, species = species1,
                                      source = source1))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))
  expect_no_error(my_intervals <- as_BirdFlowIntervals(my_bfroutes))
  expect_null(my_intervals)
})



test_that("Test Interval sampling strategy", {
  set.seed(42)

  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- species(bf)
  source1 <- "Testing"

  expect_no_error(my_routes <- Routes(fake_routes, species = species1,
                                      source = source1))
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf))

  expect_snapshot(my_routes)
  expect_snapshot(my_bfroutes)

  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes,
                                         max_n=1000,
                                         min_day_interval=7,
                                         min_km_interval=200))
  expect_snapshot(my_intervals)
  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=1,
                                         min_day_interval=7,
                                         min_km_interval=200))
  expect_equal(nrow(my_intervals$data), 1)
  expect_snapshot(my_intervals)

  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n = 3,
                                         min_day_interval = 7,
                                         min_km_interval = 200))
  expect_snapshot(my_intervals)

  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n = 3,
                                         min_day_interval = 7,
                                         min_km_interval = 20000))
  expect_snapshot(my_intervals)

  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n = 3,
                                         min_day_interval = 7,
                                         min_km_interval = 0))
  expect_snapshot(my_intervals)
})

