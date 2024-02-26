test_that("extend_birdflow() works with object", {
  bf <- BirdFlowModels::amewoo
  # Define expanded extent for example
  e <-  ext(bf)
  buffer <- 3 * res(bf)
  e[1] <- e[1] - buffer[1]
  e[2] <- e[2] + buffer[1]
  e[3] <- e[3] - buffer[2]
  e[4] <- e[4] + buffer[2]
  expect_no_error(bf2 <- extend_birdflow(bf, e))
  expect_no_error(validate_BirdFlow(bf2))

})

test_that("extend_birdflow() works with hdf5", {

  stop("Need to complete extend_birdflow() testing")
})

test_that("extend_birdflow() works with .rda", {

  stop("Need to complete extend_birdflow() testing")
})
