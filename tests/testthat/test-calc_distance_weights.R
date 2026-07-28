test_that("calc_dist_weights() works with the default 'm3' kernel", {
  w <- calc_dist_weights(dist_to_line = c(0, 500, 50000),
                         dist_along_line = c(500, 500, 500),
                         line_lengths = c(1000, 1000, 1000),
                         radius_m = 200, res_m = 1000)
  expect_length(w, 3)
  expect_true(all(w >= 0 & w <= 1))
  # Closer points get more weight
  expect_true(w[1] > w[2])
  expect_equal(w[3], 0)
})

test_that("calc_dist_weights() works with the 'bb' kernel", {
  w <- calc_dist_weights(dist_to_line = c(0, 500, 50000),
                         dist_along_line = c(500, 500, 500),
                         line_lengths = c(1000, 1000, 1000),
                         radius_m = 200, res_m = 1000, kernel = "bb")
  expect_length(w, 3)
  expect_true(all(w >= 0 & w <= 1))
  expect_true(w[1] > w[2])
  expect_equal(w[3], 0)
})

test_that("calc_dist_weights() works with the 'm1', 'm5', and 'sq' kernels", {
  # Regression test: these previously errored with
  # "Error in get(kernel) : object 'm1' not found" because the kernel
  # functions are named k_m1/k_m5/k_sq, not m1/m5/sq.
  for (k in c("m1", "m5", "sq")) {
    w <- calc_dist_weights(dist_to_line = c(0, 500, 50000),
                           dist_along_line = c(500, 500, 500),
                           line_lengths = c(1000, 1000, 1000),
                           radius_m = 200, res_m = 1000, kernel = k)
    expect_length(w, 3)
    expect_true(all(w >= 0 & w <= 1))
    expect_true(w[1] > w[2])
    expect_equal(w[3], 0)
  }
})

test_that("calc_dist_weights() errors on an invalid kernel", {
  expect_error(calc_dist_weights(dist_to_line = 0, dist_along_line = 500,
                                 line_lengths = 1000, radius_m = 200,
                                 res_m = 1000, kernel = "bogus"))
})

test_that("calc_dist_weights() gamma/kl affect the 'm3' kernel's spread", {
  args <- list(dist_to_line = 300, dist_along_line = 500,
              line_lengths = 1000, radius_m = 200, res_m = 1000)

  default <- do.call(calc_dist_weights, args)
  wider_gamma <- do.call(calc_dist_weights, c(args, list(gamma = 400000)))
  wider_kl <- do.call(calc_dist_weights, c(args, list(kl = 50)))

  expect_false(isTRUE(all.equal(default, wider_gamma)))
  expect_false(isTRUE(all.equal(default, wider_kl)))
})

test_that("calc_dist_weights_sd() keeps t/len and gamma/kl in consistent units", {
  # Regression test for a units bug where dist_along_line/line_lengths were
  # converted to km while gamma/kl were left unconverted (meters), making the
  # kernel decay ~1000x slower than intended and its contribution to sd
  # negligible next to the nugget. With consistent units, once the line is
  # long relative to kl the bridge fully decorrelates mid-line and sd should
  # approach sqrt(gamma + nugget^2), not just the nugget.
  res_m <- 1000
  gamma <- 40000
  kl <- 2000
  nugget <- res_m / 4
  len <- 20 * kl  # long enough for the kernel to fully decay mid-line

  sd_end <- calc_dist_weights_sd(0, len, res_m, kernel = "m3",
                                 gamma = gamma, kl = kl, s1 = NA)
  sd_mid <- calc_dist_weights_sd(len / 2, len, res_m, kernel = "m3",
                                 gamma = gamma, kl = kl, s1 = NA)

  expect_equal(sd_end, nugget)
  expect_equal(sd_mid, sqrt(gamma + nugget^2), tolerance = 1e-3)
})

test_that("calc_dist_weights() s1 widens the 'bb' kernel's spread", {
  # At a distance well beyond the default kernel's reach, a much larger s1
  # (wider spread) should pick up nonzero weight where the default has none.
  args <- list(dist_to_line = 20000, dist_along_line = 500,
              line_lengths = 1000, radius_m = 200, res_m = 1000,
              kernel = "bb")

  default <- do.call(calc_dist_weights, args)
  wider <- do.call(calc_dist_weights, c(args, list(s1 = 1000)))

  expect_equal(default, 0)
  expect_true(wider > default)
})
