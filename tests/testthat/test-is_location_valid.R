test_that("distr_is_valid() returns true for distributions from model", {
  bf <- BirdFlowModels::amewoo

  # Several
  distr <- get_distr(bf, 1:2, from_marginals = TRUE)
  expect_no_error(valid <- is_distr_valid(bf, distr = distr, timestep = 1:2))
  expect_true(all(valid))

  # Singular
  distr <- get_distr(bf, 1, from_marginals = TRUE)
  expect_no_error(valid <- is_distr_valid(bf, distr = distr, timestep = 1))
  expect_true(all(valid))
})

test_that("distr_is_valid() returns FALSE when it should", {
  bf <- BirdFlowModels::amewoo

  # Several
  distr <- get_distr(bf, 1:2, from_marginals = TRUE)
  a_zero_cell <- which(distr[, 1] == 0)[1]
  distr[1, 1] <- .001
  distr[, 1] <- distr[, 1] / sum(distr[, 1]) # restandarize
  expect_no_error(valid <- is_distr_valid(bf, distr = distr, timestep = 1:2))
  expect_equal(valid, c(FALSE, TRUE))

  # Singular
  distr <- distr[, 1]
  expect_no_error(valid <- is_distr_valid(bf, distr = distr, timestep = 1))
  expect_false(valid)

})

test_that("distr_is_valid() returns appropriate mask", {
  bf <- BirdFlowModels::amewoo

  # Several
  distr <- get_distr(bf, 5:7, from_marginals = TRUE)
  mask <- is_distr_valid(bf, distr, timestep = 5:7, return_mask = TRUE)
  expect_equal(nrow(distr), nrow(mask))
  expect_equal(ncol(distr), ncol(mask))
  mask2 <- distr != 0
  expect_equal(mask, mask2, ignore_attr = TRUE)

  # Singular
  distr <- get_distr(bf, 22, from_marginals = TRUE)
  mask <- is_distr_valid(bf, distr, timestep = 22, return_mask = TRUE)
  expect_equal(nrow(distr), nrow(mask))
  expect_equal(ncol(distr), ncol(mask))
  mask2 <- distr != 0
  expect_equal(mask, mask2, ignore_attr = TRUE)

})

test_that("is_location_valid() returns TRUE for valid inputs", {
  bf <- BirdFlowModels::amewoo

  # Several
  timestep <- 12
  distr <- get_distr(bf, timestep, from_marginals = TRUE)
  locs <- sample_distr(distr, n = 3)
  i <- apply(locs, 2, function(x)  which(as.logical(x)))
  x <- i_to_x(i, bf)
  y <- i_to_y(i, bf)
  date <- bf$dates$date[bf$dates$interval == timestep]

  # i , timestep
  expect_no_error(a <- is_location_valid(bf, i = i, timestep = timestep))
  expect_true(all(a))
  expect_no_error(b <- is_location_valid(bf, i = i[1], timestep = timestep))
  expect_true(b)


  # xy,  timestep
  expect_no_error(a <- is_location_valid(bf, x = x, y = y, timestep = timestep))
  expect_true(all(a))
  expect_no_error(b <- is_location_valid(bf, x = x[1], y = y[1],
                                         timestep = timestep))
  expect_true(b)


  # i, date
  expect_no_error(c <- is_location_valid(bf, i = i, date = date))
  expect_true(all(c))

  # x, y, date
  expect_no_error(d <- is_location_valid(bf, x = x, y = y, date = date))
  expect_true(all(d))

})

test_that("is_location_valid() returns FALSE when it should", {
  bf <- BirdFlowModels::amewoo

  # Several
  timestep <- 12
  distr <- get_distr(bf, which = rep(timestep, 2), from_marginals = TRUE)
  set.seed(1)
  i <- apply(distr, 2, function(x) sample(which(x == 0), 1))
  x <- i_to_x(i, bf)
  y <- i_to_y(i, bf)
  date <- bf$dates$date[bf$dates$interval == timestep]

  # i , timestep
  expect_no_error(a <-  is_location_valid(bf, i = i, timestep = timestep))
  expect_false(any(a))

  # xy,  timestep
  expect_no_error(a <- is_location_valid(bf, x = x, y = y,
                                          timestep = timestep))
  expect_false(any(a))

  # i, date
  expect_no_error(a <- is_location_valid(bf, i = i, date = date))
  expect_false(any(a))

  # x, y, date
  expect_no_error(a <- is_location_valid(bf, x = x, y = y, date = date))
  expect_false(any(a))

})
