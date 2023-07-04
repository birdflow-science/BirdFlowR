test_that("get_dates() works", {
  bf <- BirdFlowModels::rewbla
  d <- get_dates(bf)
  expect_equal(names(d), c("interval", "date", "midpoint", "start", "end", "doy"))
  expect_equal(nrow(d), n_timesteps(bf))

})
