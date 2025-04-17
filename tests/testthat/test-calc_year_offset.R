test_that("calc_year_offset works across year boundaries", {
  forward <- c(50:52, 1:4)
  expect_no_error(f_res <- calc_year_offset(forward))
  expect_equal(f_res, c(0, 0, 0, 1, 1, 1, 1))

  backward <- rev(forward)
  expect_no_error(b_res <- calc_year_offset(backward))

  expect_equal(b_res, c(0, 0, 0, 0, -1, -1, -1))

})


test_that("calc_year_offset works across multiple years", {
  forward <- rep(1:52, 3)
  expect_no_error(f_res <- calc_year_offset(forward))
  expect_equal(f_res, rep(0:2, each = 52))

  backward <- rep(52:1, 3)
  expect_no_error(b_res <- calc_year_offset(backward))
  expect_equal(b_res, rep(0:-2, each = 52))

})
