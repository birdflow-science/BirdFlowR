
test_that("lookup_timestep works with character dates", {
  bf <- BirdFlowModels::amewoo
  # Single
  expect_equal(lookup_timestep("2023-01-01", bf), 1)
  # Multiple
  expect_equal(lookup_timestep(c("2023-01-01", "2023-12-29"), bf), c(1, 52))

})


test_that("lookup_timestep works with timesteps", {
  bf <- BirdFlowModels::amewoo
  # Single
  expect_equal(lookup_timestep(1, bf), 1)
  # Multiple
  expect_equal(lookup_timestep(1:4, bf), 1:4)

  expect_error(lookup_timestep(1.4, bf))
  expect_error(lookup_timestep(53, bf))
})


test_that("lookup_timestep works with dates", {
  bf <- BirdFlowModels::rewbla

  # "Date" class
  # Single
  date <- lubridate::as_date("2022-05-10")
  expect_equal(lookup_timestep(date, bf), 19)

  # Multiple
  dates <- lubridate::as_date(c("2022-05-10", "2022-01-02"))
  expect_equal(lookup_timestep(dates, bf), c(19, 1))

  # Posixct
  date <-  as.POSIXct(date)
  expect_equal(lookup_timestep(date, bf), 19)
  dates <- as.POSIXct(dates)
  expect_equal(lookup_timestep(dates, bf), c(19, 1))

  # posixlt
  date <-  as.POSIXlt(date)
  expect_equal(lookup_timestep(date, bf), 19)
  dates <- as.POSIXlt(dates)
  expect_equal(lookup_timestep(dates, bf), c(19, 1))

})
