test_that("Timestep padding works", {

  bf <- BirdFlowModels::amewoo

  # 2. From marginals
  expect_equal(get_timestep_padding(bf), 2)

  # 3. From Transitions
  bft <- bftm <- BirdFlowR::build_transitions(bf)
  bft$marginals <- NULL
  bft$metadata$has_marginals <- FALSE
  expect_equal(get_timestep_padding(bf), 2)

  # 4. From n_timesteps()
  b <- bf
  b$marginals <- NULL
  b$metadata$has_marginals <- FALSE
  expect_equal(get_timestep_padding(b), 2)

  # 1. From metadata
  # should trump others - using wrong value to test with model that has
  # both transitions and marginals
  bftm$metadata$timestep_padding <- 3
  expect_equal(get_timestep_padding(bftm), 3)

})
