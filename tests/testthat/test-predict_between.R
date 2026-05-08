test_that("predict_between() returns matrix with correct dimensions", {
  bf <- BirdFlowModels::amewoo

  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
  start_ts <- lookup_timestep("2023-02-15", bf)
  end_ts   <- lookup_timestep("2023-05-01", bf)
  expected_steps <- length(lookup_timestep_sequence(bf, start = start_ts,
                                                    end = end_ts))

  d <- predict_between(bf,
                       x_coord = xy$x, y_coord = xy$y,
                       date    = c("2023-02-15", "2023-05-01"))

  expect_true(is.matrix(d))
  expect_equal(nrow(d), n_active(bf))
  expect_equal(ncol(d), expected_steps)
})


test_that("predict_between() distributions sum to 1", {
  bf <- BirdFlowModels::amewoo

  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
  d <- predict_between(bf,
                       x_coord = xy$x, y_coord = xy$y,
                       date    = c("2023-02-15", "2023-05-01"))

  col_sums <- colSums(d)
  expect_equal(col_sums, rep(1, ncol(d)), tolerance = 1e-10,
               ignore_attr = TRUE)
})


test_that("predict_between() pins distributions to hard observations", {
  bf <- BirdFlowModels::amewoo

  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
  i_start <- xy_to_i(xy$x[1], xy$y[1], bf)
  i_end   <- xy_to_i(xy$x[2], xy$y[2], bf)

  d <- predict_between(bf,
                       x_coord = xy$x, y_coord = xy$y,
                       date    = c("2023-02-15", "2023-05-01"))

  expect_equal(d[i_start, 1],       1, tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(d[i_end,   ncol(d)], 1, tolerance = 1e-10, ignore_attr = TRUE)
})


test_that("predict_between() returns log_z attribute", {
  bf <- BirdFlowModels::amewoo

  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)
  d <- predict_between(bf,
                       x_coord = xy$x, y_coord = xy$y,
                       date    = c("2023-02-15", "2023-05-01"))

  expect_false(is.null(attr(d, "log_z")))
  expect_true(is.numeric(attr(d, "log_z")))
  expect_true(attr(d, "log_z") < 0)  # log prob of a specific path is negative
})


test_that("predict_between() works with soft potentials", {
  bf <- BirdFlowModels::amewoo

  ts <- lookup_timestep(c("2023-02-15", "2023-05-01"), bf)
  obs_matrix <- matrix(runif(n_active(bf) * 2), nrow = n_active(bf))
  colnames(obs_matrix) <- paste0("t", ts)

  expect_no_error(d <- predict_between(bf, potentials = obs_matrix))
  expect_true(is.matrix(d))
  expect_equal(colSums(d), rep(1, ncol(d)), tolerance = 1e-10,
               ignore_attr = TRUE)
})


test_that("predict_between() errors on bad input", {
  bf <- BirdFlowModels::amewoo
  xy <- latlon_to_xy(lat = c(30.5, 45.5), lon = c(-91.5, -68.5), bf)

  expect_error(
    predict_between(bf, x_coord = xy$x, y_coord = xy$y,
                    date = c("2023-02-15", "2023-05-01"),
                    potentials = matrix(1, nrow = n_active(bf), ncol = 2)),
    regexp = "not both"
  )
  expect_error(predict_between(bf), regexp = "Must provide")
  expect_error(
    predict_between(bf, x_coord = xy$x, y_coord = xy$y),
    regexp = "date is required"
  )
})


test_that("predict_between() end marginal matches predict() masked by end potential", {
  # With binary masks at both start and end, the marginal at the end timestep
  # from predict_between() should equal predict() forward from the start mask,
  # then masked by the end mask and renormalized. These are exact (no Monte
  # Carlo noise), so we use a tight numerical tolerance.

  bf <- BirdFlowModels::amewoo
  start_ts <- 9
  end_ts   <- 20

  # Binary masks covering top 60% of the marginal at each endpoint
  make_mask <- function(ts) {
    m <- get_distr(bf, ts)
    s <- sort(m, decreasing = TRUE)
    thresh <- s[min(which(cumsum(s) >= 0.6))]
    as.numeric(m >= thresh)
  }
  mask_start <- make_mask(start_ts)
  mask_end   <- make_mask(end_ts)

  obs_matrix <- cbind(mask_start, mask_end)
  colnames(obs_matrix) <- paste0("t", c(start_ts, end_ts))

  d_between <- predict_between(bf, potentials = obs_matrix)
  end_col <- ncol(d_between)

  # Theoretical: predict() from normalized start mask, then apply end mask
  distr_start <- mask_start / sum(mask_start)
  d_predict <- predict(bf, distr_start, start = start_ts, end = end_ts)
  theoretical <- d_predict[, end_col, drop = TRUE] * mask_end
  theoretical <- theoretical / sum(theoretical)

  expect_equal(unname(d_between[, end_col]), unname(theoretical),
               tolerance = 1e-8)
})


test_that("predict_between() marginal matches forward filter with unconstrained end", {
  # With a constrained start and unconstrained end (uniform potential), the
  # marginals from predict_between() should match predict() run forward from
  # the same start distribution.

  bf <- BirdFlowModels::amewoo
  start_ts <- 9
  end_ts   <- 20

  # Start potential: top 60% of marginal probability mass
  marginal <- get_distr(bf, start_ts)
  sorted_probs <- sort(marginal, decreasing = TRUE)
  threshold <- sorted_probs[min(which(cumsum(sorted_probs) >= 0.6))]
  mask_start <- as.numeric(marginal >= threshold)

  # Uniform (unconstrained) end potential
  obs_matrix <- cbind(mask_start, rep(1, n_active(bf)))
  colnames(obs_matrix) <- paste0("t", c(start_ts, end_ts))

  d_between <- predict_between(bf, potentials = obs_matrix)

  # Theoretical: predict() forward from the normalized start mask
  distr_start <- mask_start / sum(mask_start)
  d_predict <- predict(bf, distr_start, start = start_ts, end = end_ts)

  # The two should be proportional at every timestep.
  # Compare by correlation — should be very high (>0.999).
  for (col in seq_len(ncol(d_between))) {
    ts_label <- colnames(d_between)[col]
    r <- cor(d_between[, col],
             d_predict[, col, drop = TRUE])
    expect_gt(r, 0.999,
              label = paste0("correlation at ", ts_label))
  }
})
