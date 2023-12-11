test_that("get_dates() works", {
  bf <- BirdFlowModels::rewbla
  d <- get_dates(bf)
  expect_equal(names(d), c("timestep", "date", "label", "julian", "week"))
  expect_equal(nrow(d), n_timesteps(bf))
  expect_equal(lapply(d, class), list(timestep = "numeric",
                                     date = "character",
                                     label = "character",
                                     julian = "numeric",
                                     week = "integer"))
  expect_s3_class(d, "data.frame")


})
