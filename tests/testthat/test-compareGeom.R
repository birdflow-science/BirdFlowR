test_that("compareGeom() works", {
  bf1 <- BirdFlowModels::amewoo
  bf2 <- BirdFlowModels::rewbla

  # Two BirdFlow objects
  expect_error(compareGeom(bf1, bf2), regexp = "extents do not match")
  expect_true(compareGeom(bf1, bf1))

  # One bf and one terra
  r1a <- rast(bf1, 1) # single band
  r1b <- rast(bf1, 1:3) # multi band

  # Matching
  expect_true(compareGeom(bf1, r1a))
  expect_true(compareGeom(bf1, r1b))
  expect_true(compareGeom(r1a, bf1))

  # Not matching
  expect_error(compareGeom(bf2, r1a), "extents do not match")
  expect_error(compareGeom(r1b, bf2), "extents do not match")

})
