test_that("truncation works accross year boundary", {

  # Truncation across year boundary
  bf <- BirdFlowModels::amewoo # need circular
  expect_no_error(tbf <- truncate_birdflow(bf, start = 40, end = 10))

  if (interactive()) {
    r <- route(tbf, season = "all")
    plot(r)  # color guide is wrong.
  }
})


test_that("truncation works", {

  bf <- BirdFlowModels::amewoo
  dates1 <- get_dates(bf)
  expect_no_error(tbf <- truncate_birdflow(bf, start = 5, end = 10))
  dates2 <- get_dates(tbf)

  expect_equal(dates1[5:10, -1], dates2[, -1], ignore_attr = TRUE)

  expect_equal(get_distr(bf, 5), get_distr(tbf, 1))

  expect_equal(get_dynamic_mask(bf, 5), get_dynamic_mask(tbf, 1))

  expect_equal(n_timesteps(tbf), 6)
  expect_equal(is_cyclical(tbf), FALSE)
  expect_equal(n_transitions(tbf), 5)
  expect_equal(lookup_timestep_sequence(tbf), 1:6)

})
