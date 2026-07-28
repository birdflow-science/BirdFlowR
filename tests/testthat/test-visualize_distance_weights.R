test_that("visualize_distance_weights() works with type = 'envelope'", {
  p <- NULL
  expect_no_error(p <- visualize_distance_weights(type = "envelope"))
  expect_s3_class(p, "ggplot")

  p_bb <- NULL
  expect_no_error(
    p_bb <- visualize_distance_weights(type = "envelope", kernel = "bb",
                                       s1 = 20))
  expect_s3_class(p_bb, "ggplot")
})

test_that("visualize_distance_weights() works with type = 'raster'", {
  p <- NULL
  expect_no_error(p <- visualize_distance_weights(type = "raster", n = 10))
  expect_s3_class(p, "ggplot")

  p_bb <- NULL
  expect_no_error(
    p_bb <- visualize_distance_weights(type = "raster", kernel = "bb",
                                       s1 = 20, n = 10))
  expect_s3_class(p_bb, "ggplot")
})

test_that("visualize_distance_weights() errors on invalid kernel or type", {
  expect_error(visualize_distance_weights(kernel = "bogus"))
  expect_error(visualize_distance_weights(type = "bogus"))
})
