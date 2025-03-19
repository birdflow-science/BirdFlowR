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
  min_km <- 200
  min_days <- 7

  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes,
                                         max_n=1000,
                                         min_day_interval = min_days,
                                         min_km_interval= min_km))

  # This test is failing because the second half of the function doesn't
  # honor the valid pairs identified in the first half.
  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1) ^2)) / 1000
  lag <- with(my_intervals$data, date2 - date1) |> as.numeric(units = "days")
  expect_true(all(dist_km >= min_km))
  expect_true(all(lag >= min_days))

  # Number of routes (1)
  # Note this fails if run repeatedly but doesn't always fail because
  # random sampling sometimes selects valid intervals
  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n=1,
                                         min_day_interval= min_days,
                                         min_km_interval= min_km))
  expect_equal(nrow(my_intervals$data), 1)
  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1) ^2)) / 1000
  lag <- with(my_intervals$data, date2 - date1) |> as.numeric(units = "days")
  expect_true(all(dist_km >= min_km))
  expect_true(all(lag >= min_days))


  # Number of routes (3)
  # Note this fails if run repeatedly but doesn't always fail because
  # random sampling sometimes selects valid intervals
  expect_no_error(
    my_intervals <- as_BirdFlowIntervals(my_bfroutes, max_n = 3,
                                         min_day_interval = min_days,
                                         min_km_interval = min_km))

  expect_equal(nrow(my_intervals$data), 3)

  dist_km <- with(my_intervals$data, sqrt((x2 - x1)^2 + (y2 - y1) ^2)) / 1000
  lag <- with(my_intervals$data, date2 - date1) |> as.numeric(units = "days")
  expect_true(all(dist_km >= min_km))
  expect_true(all(lag >= min_days))


})



## More to add -- print, etc
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




test_that("Routes() -> as_BirdFlowRoutes() with different aggregations works", {
  set.seed(42)
  bf <- BirdFlowModels::amewoo

  fake_tracks <- make_fake_tracking_data(bf, 5,
                                         start = 10,
                                         end = 15,
                                         interval =
                                           as.difftime(.2, units = "days"),
                                         bandwidth = 20,
                                         year_sd = 0)
  species1 <- "amewoo"
  source1 <- "eBird"
  expect_no_error(
    my_routes <- Routes(fake_tracks, species = species1, source = source1))

  expect_equal(nrow(fake_tracks), nrow(my_routes$data))


  data <- my_routes$data
  data$timestep <- lookup_timestep(data$date, bf = bf)
  data <- cbind(data,
                latlon_to_xy(data[, c("lat", "lon")], bf = bf))

  data$is_valid <- is_location_valid(bf, x = data$x, y = data$y,
                                     date = data$date)
  cols <- c("route_id", "x", "y", "date")

  # Mean
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf,
                                                   aggregate='mean'))


  means <- data |>
    dplyr::group_by(.data$route_id, .data$timestep) |>
    dplyr::summarize(x = mean(x),
              y = mean(y),
              date = mean(date)) |>
    as.data.frame()

  expect_equal(my_bfroutes$data[, cols], means[, cols])


  # Median
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf,
                                                   aggregate = "median"))

  medians <- my_bfroutes$data |>
    dplyr::group_by(.data$route_id, .data$timestep) |>
    dplyr::summarize(x = median(x),
                     y = median(y),
                     date = median(date)) |>
    as.data.frame()
  expect_equal(my_bfroutes$data[, cols], medians[, cols])


  # Midweek
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf,
                                                   aggregate='midweek'))

  # Multiple observations per day so midweek observation should always be
  # on the midweek day
  result_dates <- my_bfroutes$data$date |> lubridate::as_date()
  expected_dates <- my_bfroutes$data$timestep |> lookup_date(bf = bf)
  expect_equal(result_dates, expected_dates)



  set.seed(1)
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes, bf=bf,
                                                   aggregate='random'))

  expect_snapshot(my_bfroutes$data[1:10, c("route_id", "i", "timestep")])


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

