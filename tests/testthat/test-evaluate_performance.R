test_that("evaluate performance is consistent", {
  bf <- BirdFlowModels::amewoo
  expect_warning(stats <- evaluate_performance(bf))
  expect_snapshot(stats)

  expect_warning(stats2 <- evaluate_performance(bf, distr_only = TRUE))
  expect_equal(stats$min_distr_cor, stats2$min_distr_cor)
  expect_equal(stats$mean_distr_cor, stats2$mean_distr_cor)

})
