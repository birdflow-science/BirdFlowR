test_that("build_transitions(0) and drop_transitions() work", {

  # Add and drop transitions and see if you end up with an identical object
  bf <- BirdFlowModels::amewoo
  tbf <- build_transitions(bf)
  bf2 <- drop_transitions(tbf)
  expect_equal(bf, bf2)
  expect_true(has_transitions(tbf))

  # Delete marginals from object with transitions
  tbf$marginals <- NULL
  tbf$metadata$has_marginals <- FALSE
  # But transitions still the same
  expect_equal(get_transition(bf, "T_01-02"), get_transition(tbf, "T_01-02"))


})
