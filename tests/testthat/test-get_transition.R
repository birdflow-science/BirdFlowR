test_that("get_transition() is consistent for forward transitions", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(f1 <- get_transition(bf, "T_01-02"))
  h <- hist(as.numeric(f1), plot = FALSE,
            breaks = c(seq(0, 0.0001, 0.0001),
                       seq(0.002, 0.01, 0.001),
                       seq(0.02, .1, 0.02),
                       seq(0.2, 1, 0.4) ))
  histogram_counts <- data.frame(upper_bound = h[["breaks"]][-1],
                                 count = h[["counts"]])
  expect_snapshot(histogram_counts)
})

test_that("get_transition() is consistent for backwards transitions", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(b1 <- get_transition(bf, "T_02-01"))
  h <- hist(as.numeric(b1), plot = FALSE,
            breaks = c(seq(0, 0.0001, 0.0001),
                       seq(0.002, 0.01, 0.001),
                       seq(0.02, .1, 0.02),
                       seq(0.2, 1, 0.4) ))
  histogram_counts <- data.frame(upper_bound = h[["breaks"]][-1],
                                 count = h[["counts"]])
  expect_snapshot(histogram_counts)
})


test_that("get_transition() produces error with missing transition", {
  bf <- BirdFlowModels::amewoo
  expect_error(t <- get_transition(bf, "T_52-53"),
               "There is no marginal for transition T_52-53")
})


test_that("get_transition() throws error when there are no transitions", {
  bf <- new_BirdFlow()
  expect_error(t <- get_transition(bf, "T_01-02"),
               "The BirdFlow object should have either transitions or marginals")
})

test_that("forward and backwards transitions are consistent", {
  # Here we project the first timestep's distribution (d1a) forward one step
  # to generate a timestep 2 distribution (d2); and then project that back one
  # step to generate a new timestep 1 distribution (d1b).  We expect
  #  the two, timestep 1, distributions to be highly correlated.
  bf <- BirdFlowModels::amewoo
  d1a <- get_distr(bf, 1)
  ft <- get_transition(bf, "T_01-02")
  bt <- get_transition(bf, "T_02-01")
  d2 <- ft %*% d1a
  d1b <- as.numeric(bt %*% d2)
  expect_true(cor(d1a, d1b) > 0.999)
})

test_that("transition_from_marginal throws errors with bogus direction", {
  bf <- BirdFlowModels::amewoo
  expect_error(transition_from_marginal(bf$marginals[["M_01-02"]],
               direction = "Bleh"), "Direction must be forward or backward" )
})

test_that("get_transition returns same values from sparse and standard marginals", {
  # As of 0.0.0.9044 we use different calculations on sparse vs standard marg.
  sparse_bf <- BirdFlowModels::amewoo
  full_bf <- sparse_bf
  # Convert first three marginals to standard matricies
  for(marg in c("M_01-02", "M_02-03", "M_03-04")){
    full_bf$marginals[marg] <- as.matrix(full_bf$marginals[marg])
  }
  # Forward
  expect_equal(as.matrix(get_transition(sparse_bf, "T_01-02")),
               as.matrix(get_transition(full_bf, "T_01-02")))
  # Backward
  expect_equal(as.matrix(get_transition(sparse_bf, "T_02-01")),
               as.matrix(get_transition(full_bf, "T_02-01")))
})



