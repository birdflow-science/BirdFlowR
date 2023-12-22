test_that("combine_transitions is consistent with predict", {

  bf <- BirdFlowModels::amewoo

  start <- 5
  end <- 49
  direction <- "backward"

  # Combine the transitions
  expect_no_error(
    trans <- combine_transitions(bf, start = start,
                                 end = end, direction = direction)
  )

  # predict ending distribution with bf
  # - by stepping through intervening transitions
  s_st <- get_distr(bf, start)
  e_st <- get_distr(bf, end) # status and trends
  p <- predict.BirdFlow(bf, s_st, start = start,
                        end = end, direction = direction)
  e_bf <- p[, ncol(p)] # end distribution from Birdflow predict (active cells)

  start_dm <- get_dynamic_mask(bf, start)
  end_dm <- get_dynamic_mask(bf, end)

  # end distribution from consolidated transition (just dynamic)
  e_trans <- as.vector(trans %*% s_st[start_dm])

  # They should be identical
  expect_equal <- cor(e_trans, e_bf[end_dm])


})
