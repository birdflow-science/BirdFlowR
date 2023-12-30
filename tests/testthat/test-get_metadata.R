test_that("get_metadata() errors nicely with wrong input length", {
  bf <- BirdFlowModels::amewoo
  expect_error(get_metadata(bf, c('n_transitions', 'n_timesteps')),
               '^what should be one of')
})
