test_that("Routes() -> as_BirdFlowRoutes() with different aggregations works", {
  set.seed(42)
  bf <- BirdFlowModels::amewoo

  fake_tracks <- make_fake_tracking_data(bf, 5,
    start = 10,
    end = 15,
    interval =
      as.difftime(.2, units = "days"),
    bandwidth = 20,
    year_sd = 0
  )
  species1 <- "amewoo"
  source1 <- "eBird"
  expect_no_error(
    my_routes <- Routes(fake_tracks, species = species1, source = source1)
  )

  expect_equal(nrow(fake_tracks), nrow(my_routes$data))


  data <- my_routes$data
  data$timestep <- lookup_timestep(data$date, bf = bf)
  data <- cbind(
    data,
    latlon_to_xy(data[, c("lat", "lon")], bf = bf)
  )

  data$is_valid <- is_location_valid(bf,
    x = data$x, y = data$y,
    date = data$date
  )
  cols <- c("route_id", "x", "y", "date")

  # Mean
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes,
    bf = bf,
    aggregate = "mean"
  ))


  means <- data |>
    dplyr::group_by(.data$route_id, .data$timestep) |>
    dplyr::summarize(
      x = mean(x),
      y = mean(y),
      date = mean(date)
    ) |>
    as.data.frame()

  expect_equal(my_bfroutes$data[, cols], means[, cols])


  # Median
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes,
    bf = bf,
    aggregate = "median"
  ))

  medians <- my_bfroutes$data |>
    dplyr::group_by(.data$route_id, .data$timestep) |>
    dplyr::summarize(
      x = median(x),
      y = median(y),
      date = median(date)
    ) |>
    as.data.frame()
  expect_equal(my_bfroutes$data[, cols], medians[, cols])


  # Midweek
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes,
    bf = bf,
    aggregate = "midweek"
  ))

  # Multiple observations per day so midweek observation should always be
  # on the midweek day
  result_dates <- my_bfroutes$data$date |> lubridate::as_date()
  expected_dates <- my_bfroutes$data$timestep |> lookup_date(bf = bf)
  expect_equal(result_dates, expected_dates)



  set.seed(1)
  expect_no_error(my_bfroutes <- as_BirdFlowRoutes(my_routes,
    bf = bf,
    aggregate = "random"
  ))
  my_bfroutes$data$stay_len <- my_bfroutes$data$stay_len %/% 7
  tail(my_bfroutes$data, 20)

  expect_snapshot(my_bfroutes$data[1:10, c("route_id", "i", "timestep")])
})
