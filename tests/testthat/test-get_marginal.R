test_that("get_marginal() works", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  expect_no_condition(m <- get_marginal(bf, "M_52-01"))
  expect_true(is.matrix(m))

  m2 <- get_marginal(bf, from = 52)


})
