test_that("lookup dates works with timesteps", {
  bf <- BirdFlowModels::amewoo

  first_ten <-  as.Date(get_dates(bf)$date[1:10])
  expect_equal(lookup_date(1:10, bf), first_ten)
  expect_equal(lookup_date(paste0("T", 1:10), bf), first_ten)

  # All NA
  expect_equal(lookup_date(NA, bf), as.Date(NA))
  expect_equal(lookup_date(NA_character_, bf), as.Date(NA))
  expect_equal(lookup_date(NA_real_, bf), as.Date(NA))

  # With NA
  expect_equal(lookup_date(c(1:10, NA), bf), c(first_ten, NA))
  expect_equal(lookup_date(c(paste0("T", 1:10), NA), bf), c(first_ten, NA))

})


test_that("lookup dates works with transitions and marginals", {
  bf <- BirdFlowModels::amewoo

  trans <- lookup_transitions(bf, start = 51, end = 3, direction = "forward")
  d <- lookup_date(trans, bf)
  expect_snapshot(d)

  # with NA
  expect_equal(lookup_date(c(trans, NA), bf), c(d, NA))

  # backwards
  trans <- lookup_transitions(bf, start = 3, end = 51, direction = "backward")
  backwards <- lookup_date(trans, bf)
  expect_equal(backwards, rev(d))


})
