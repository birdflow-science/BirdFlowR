test_that("plot_movement_vectors runs cleanly", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo
  expect_no_error(mv <- plot_movement_vectors(bf, 5))
})
