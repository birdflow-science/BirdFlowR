test_that("plot_movement_vectors runs cleanly", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(mv <- plot_movement_vectors(bf, 5))
  expect_no_error(print(mv))
})
