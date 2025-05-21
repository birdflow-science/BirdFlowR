test_that("Interval validation works", {
  set.seed(42)

  # Setup
  fake_routes <- make_fake_routes()
  bf <- BirdFlowModels::amewoo
  species1 <- bf$species
  source1 <- "Testing"

  my_routes <- Routes(fake_routes,
    species = species1,
    source = source1
  )
  my_bfroutes <- as_BirdFlowRoutes(my_routes, bf = bf)

  # Constraints
  min_day <- 7
  max_day <- 180
  min_km <- 200
  max_km <- 8000

  my_intervals <- BirdFlowR::as_BirdFlowIntervals(my_bfroutes,
    max_n = 1000,
    min_day_interval = min_day,
    max_day_interval = max_day,
    min_km_interval = min_km,
    max_km_interval = max_km
  )

  expect_no_error(eval_res <- calculate_interval_metrics(my_intervals, bf))
  single_value_outputs <- eval_res[[1]]
  expect_true(single_value_outputs['n_intervals']>0)
  transition_level_outputs <- eval_res[[2]]
  expect_true(nrow(transition_level_outputs)>0)
})

