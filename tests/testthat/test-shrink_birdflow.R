test_that("shrink birdflow doesn't change original models", {
  bf1 <- BirdFlowModels::amewoo
  expect_no_error(bf2 <-  shrink_birdflow(bf1))
  expect_equal(bf1,  bf2)
})

test_that("shrink birdflow reverts extended BirdFlow to original", {
  bf1 <- BirdFlowModels::amewoo
  cell_buffer <- 2
  buffer <- xres(bf1) * cell_buffer # converted to map units (m)
  e <-  ext(bf1) |> buffer_extent(buffer = buffer)
  bf2 <- extend_birdflow(bf1, e)
  expect_no_error(bf3 <- shrink_birdflow(bf2))
  expect_equal(bf1, bf3)
})


test_that("shrink birdflow doesn't drop internal columns or rows", {
  bf1 <- BirdFlowModels::amewoo
  bf1$geom$mask[, 3] <- FALSE  # hacked and broken bf but will work for test
  bf1$geom$mask[2, ] <- FALSE  # hacked and broken bf but will work for test
  bf2 <- shrink_birdflow(bf1)
  expect_equal(bf1, bf2)
})
