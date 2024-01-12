test_that("BirdFlowModels::amewoo validates", {
  expect_no_error(validate_BirdFlow(BirdFlowModels::amewoo))
})

test_that("validate_BirdFlow works with allow_incompletes = TRUE", {
  bf <- BirdFlowModels::amewoo
  bf$marginals <- NULL
  bf$metadata$has_marginals <- FALSE
  expect_error(validate_BirdFlow(bf),
               "model has neither transitions nor marginals")
  expect_no_error(validate_BirdFlow(bf, allow_incomplete = TRUE))
})


test_that("validate_BirdFlow throws expected errors", {
  bf <- BirdFlowModels::amewoo

  # Wrong class
  expect_error(validate_BirdFlow(NA), "not a BirdFlow object")

  # Inconsistent marginal names
  bad <- bf
  names(bad$marginals)[1:2] <- c("a", "b")
  expect_error(validate_BirdFlow(bad), "marginal names don't match index")

  # Inconsistent marginal dimensions
  bad <- bf
  bad$marginals[[1]] <- bad$marginals[[1]][-1, ]
  expect_error(validate_BirdFlow(bad),
               "marginal dimensions inconsistent with dynamic_mask")

  # Inconsistent transition dimensions
  bad <- build_transitions(bf)
  bad$transitions[[1]] <- bad$transitions[[1]][-1, ]
  expect_error(validate_BirdFlow(bad),
               "transition matrix dimensions inconsistent with dynamic_mask")

  # Geom not a list
  bad <- bf
  bad$geom <- 1
  expect_error(validate_BirdFlow(bad))

  # Geom missing elements
  bad <- bf
  bad$geom <- list()
  expect_error(validate_BirdFlow(bad), "x\\$geom missing")

  # Missing dates or date component
  bad <- bf
  bad$dates$date <- NULL
  expect_error(validate_BirdFlow(bad), "x\\$dates is missing columns")
  bad$dates <- NULL
  expect_error(validate_BirdFlow(bad), "x missing:dates")

  # Dates and distributions not consistent
  bad <- bf
  bad$distr <- bad$distr[, -1]
  expect_error(validate_BirdFlow(bad),
               "do not represent the same number of timesteps")

  # Marginals don't sum to 1
  bad <- bf
  bad$marginals[[1]][1, 1] <- 4
  expect_error(validate_BirdFlow(bad),
               "Not all marginals have a sum of one")

  # Distributions don't sum to 1
  bad <- bf
  bad$distr[ , 1] <- 0
  expect_error(validate_BirdFlow(bad),
               "not all distributions sum to one")


  # Empty dynamic mask (for any timestep)
  bad <- bf
  bad$geom$dynamic_mask[ , 1] <- 0
  expect_error(validate_BirdFlow(bad),
               "dynamic mask eliminates all cells for some timesteps")


})
