test_that("predicting preserves distribution over 5 weeks", {
  bf <- BirdFlowModels::amewoo
  if(!has_dynamic_mask(bf))
    bf <- BirdFlowR:::add_dynamic_mask(bf)

  d1 <- get_distr(bf, 1)
  p <- predict(bf, d1, start = 1, end = 5)
  pred_d5 <- p[, 5]
  d5 <- get_distr(bf, 5)

  expect_gt(cor(d5, pred_d5), expected = .97)

  p <- predict(bf, d5, start = 5, end = 1, direction = "backward")
  pred_d1 <- p[, 5]

  expect_gt(cor(d1, pred_d1), expected = .97)

})



test_that("predict() is consistent with full and sparse marginals", {
  # This refers to the format of the objects not the content.
  sparse_bf <- BirdFlowModels::amewoo
  if(!has_dynamic_mask(sparse_bf))
    sparse_bf <- add_dynamic_mask(sparse_bf)
  full_bf <- sparse_bf
  for (marg in (c("M_01-02", "M_02-03", "M_03-04", "M_04-05"))) {
    full_bf$marginals[[marg]] <- as.matrix(full_bf$marginals[[marg]])
  }
  distr <- get_distr(sparse_bf, 1)
  expect_no_error(sparse_pred <- predict( sparse_bf, distr, start = 1, end = 3))
  expect_no_error(full_pred <-  predict( full_bf, distr, start = 1, end = 3))
  expect_equal(full_pred, sparse_pred)

})

test_that("predict() is consistent with marginals and transitions", {
  bf <- BirdFlowModels::amewoo
  if(!has_dynamic_mask(bf))
    bf <- add_dynamic_mask(bf)
  t_bf <- build_transitions(bf)
  distr <- get_distr(bf, 1)
  expect_no_error(pred <- predict( bf, distr, start = 1, end = 3))
  expect_no_error(t_pred <-  predict( t_bf, distr, start = 1, end = 3))
  expect_equal(pred, t_pred)
})
