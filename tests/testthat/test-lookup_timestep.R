
# Make psuedo bf object with dates component

bf <- new_BirdFlow()
bf$dates <- structure(list(
  doy = c(4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5,
          53.5, 60.5, 67.5, 74.5, 81.5, 88.5, 95.5, 102.5, 109.5, 116.5,
          123.5, 130.5, 137.5, 144.5, 151.5, 158.5, 165.5, 172.5, 179.5,
          186.5, 193.5, 200.5, 207.5, 214.5, 221.5, 228.5, 235.5, 242.5,
          249.5, 256.5, 263.5, 270.5, 277.5, 284.5, 291.5, 298.5, 305.5,
          312.5, 319.5, 326.5, 333.5, 340.5, 347.5, 354.5, 361.5),
  interval = 1:52,
  date =
    structure(c(17900, 17907, 17914, 17921, 17928, 17935,
                17942, 17949, 17956, 17963, 17970, 17977, 17984, 17991, 17998,
                18005, 18012, 18019, 18026, 18033, 18040, 18047, 18054, 18061,
                18068, 18075, 18082, 18089, 18096, 18103, 18110, 18117, 18124,
                18131, 18138, 18145, 18152, 18159, 18166, 18173, 18180, 18187,
                18194, 18201, 18208, 18215, 18222, 18229, 18236, 18243, 18250,
                18257), class = "Date")),
  class = "data.frame", row.names = c(NA, -52L))



test_that("lookup_timestep works with character dates", {
  # Note bf created at top of this test document

  # Single
  expect_equal(lookup_timestep("2023-01-01", bf), 1)
  # Multiple
  expect_equal(lookup_timestep(c("2023-01-01", "2023-12-29"), bf), c(1, 52))

})


test_that("lookup_timestep works with timesteps", {
  # Note bf created at top of this test document

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
