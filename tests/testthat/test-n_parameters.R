test_that("n_parameters() works", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(np <- n_parameters(bf))

  # Calculate n_param from marginals
  mar <- bf$marginals
  mar$index <- NULL
  np2 <- sum(sapply(mar, function(x) prod(dim(x))), get_dynamic_mask(bf, 1))
  expect_equal(np, np2)

  # Hack to check bf without dynamic mask
  bf$geom$dynamic_mask <- NULL
  expect_no_error(full_np <- n_parameters(bf))
  full_np2 <- n_active(bf)^2 * 52 + n_active(bf)
  expect_equal(full_np, full_np2)

  # Truncated Model
  bf <- BirdFlowModels::amewoo
  tbf <- truncate_birdflow(bf, start = 10, end = 15)
  expect_no_error(tnp <- n_parameters(bf))
  tnp2 <-    sum(sapply(mar, function(x) prod(dim(x))), get_dynamic_mask(bf, 1))
  expect_equal(tnp, tnp2)
})
