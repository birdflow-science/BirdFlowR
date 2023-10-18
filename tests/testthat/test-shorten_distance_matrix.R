test_that("shortening then expanding a distance matrix reproduces original", {
  # Create distance matrix
  x <- runif(5, 1, 100)
  y <- runif(5, 1, 100)
  dm <- as.matrix(dist(cbind(x, y)))

  # Test
  expect_no_condition(s <- shorten_distance_matrix(dm))
  expect_no_condition(dm2 <- expand_distance_matrix(s))
  expect_equal(dm, dm2, ignore_attr = TRUE)

})
