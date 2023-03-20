test_that("predicting preserves distribution over 5 weeks", {
  bf <- BirdFlowModels::amewoo
  d1 <- get_distr(bf, 1)
  p <- predict(bf, d1, 1, 5)
  pred_d5 <- p[, 5]
  d5 <- get_distr(bf, 5)

  expect_gt(cor(d5, pred_d5), expected = .97)

  p <- predict(bf, d5, 5, 1, "backward")
  pred_d1 <- p[, 5]

  expect_gt(cor(d1, pred_d1), expected = .97)

})

