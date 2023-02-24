test_that("BirdFlowModels::amewoo validates", {
  expect_no_error(validate_BirdFlow(BirdFlowModels::amewoo))
})

test_that("validate_BirdFlow works with allow_incompletes = TRUE",{
  bf <- BirdFlowModels::amewoo
  bf$marginals <- NULL
  bf$metadata$has_marginals <- FALSE
  expect_error(validate_BirdFlow(bf))
  expect_no_error(validate_BirdFlow(bf, allow_incomplete = TRUE))
})
