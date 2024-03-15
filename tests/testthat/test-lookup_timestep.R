test_that("lookup_timestep works with NAs", {
  bf <- BirdFlowModels::amewoo
  nice_error <- "^Date lookup failed"
  # Single (logical) NA, which may be input by user
  expect_error(lookup_timestep(NA, bf), nice_error)
  expect_equal(lookup_timestep(NA, bf, allow_failure = TRUE), NA_real_)
  # Single character NA
  expect_error(lookup_timestep(NA_character_, bf), nice_error)
  expect_equal(lookup_timestep(NA_character_, bf, allow_failure = TRUE),
               NA_real_)
  # Single numeric NA
  expect_error(lookup_timestep(NA_real_, bf), nice_error)
  expect_equal(lookup_timestep(NA_real_, bf, allow_failure = TRUE), NA_real_)
  # NA in t# style character vector
  expect_error(lookup_timestep(c("t1", NA_character_), bf), nice_error)
  expect_equal(lookup_timestep(c("t1", NA_character_), bf,
                               allow_failure = TRUE),
               c(1, NA_real_))
  # NA in numeric vector
  expect_error(lookup_timestep(c(1, NA_real_), bf), nice_error)
  expect_equal(lookup_timestep(c(1, NA_real_), bf,
                               allow_failure = TRUE),
               c(1, NA_real_))
  # NA in Date vector
  expect_error(lookup_timestep(as.Date(c("2023-01-01", NA_character_)), bf),
               nice_error)
  expect_equal(lookup_timestep(as.Date(c("2023-01-01", NA_character_)), bf,
                               allow_failure = TRUE),
               c(1, NA_real_))
})


test_that("lookup_timestep works with character dates", {

  bf <- BirdFlowModels::amewoo
  # Single
  expect_equal(lookup_timestep("2023-01-01", bf), 1)
  # Multiple
  expect_equal(lookup_timestep(c("2023-01-01", "2023-12-29"), bf), c(1, 52))

  # Check with alternate date format
  bf <- change_date_format(bf)
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

  # Check with alternate date format
  bf <- change_date_format(bf)

  expect_equal(lookup_timestep(1, bf), 1)
  # Multiple
  expect_equal(lookup_timestep(1:4, bf), 1:4)

  expect_error(lookup_timestep(1.4, bf))
  expect_error(lookup_timestep(53, bf))


})


test_that("lookup_timestep works with dates", {
  bf <- BirdFlowModels::amewoo

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

  # Check with alternate date format
  bf <- change_date_format(bf)
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
