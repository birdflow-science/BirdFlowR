test_that("marginal_stats and related functions work", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(ms <- marginal_stats(bf))
  expect_no_error(s <- sum_marginals(bf))
  expect_no_error(pz <- calc_pct_zero(bf))
  expect_equal(ms$sum, s)
  expect_equal(ms$pct_zero, pz)
})
