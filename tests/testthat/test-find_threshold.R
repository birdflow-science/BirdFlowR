test_that("man/find_threshold() works", {
  x <- rep(1e4, 100) + seq(0.01, 1, .01)
  x <- 1:1000
  p <- .90
  expect_no_error(t <- find_threshold(x = x, p = p))

  # Sum of all values greater than or equal to threshold is greater than p
  expect_true(sum(x[x >= t]) / sum(x) > p)

  # With next highest value it's no longer true
  t2 <- min(x[x > t])
  expect_true(sum(x[x >= t2]) / sum(x) < p)
})
