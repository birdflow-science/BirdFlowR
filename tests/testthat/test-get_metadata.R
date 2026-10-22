test_that("get_metadata() errors nicely with wrong input length", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo
  expect_error(get_metadata(bf, c("n_transitions", "n_timesteps")),
               "^what should be one of")
})
