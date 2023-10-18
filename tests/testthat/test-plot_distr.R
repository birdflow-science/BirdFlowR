test_that("plot_distr() works with default parameters", {
  bf <- BirdFlowModels::amewoo

  ## Default parameters

  # 1. single plot
  expect_no_error(p1 <- plot_distr(get_distr(bf, 1), bf))
  expect_no_error(print(p1))

  # 2 facet
  expect_no_error(p2 <- plot_distr(get_distr(bf, 1:3), bf))
  expect_no_error(print(p2))
})

test_that("plot_distr() works with dynamic mask", {
  bf <- BirdFlowModels::amewoo

  ## With dynamic mask

  # 3. single plot
  expect_no_error(p3 <- plot_distr(get_distr(bf, 1), bf,
                                   show_dynamic_mask = TRUE))
  expect_no_error(print(p3))

  # 4 facet
  expect_no_error(p4 <- plot_distr(get_distr(bf, 1:3), bf,
                                   show_dynamic_mask = TRUE))
  expect_no_error(print(p4))

})

test_that("plot_distr() works with llimits and dynamic scaling", {
  bf <- BirdFlowModels::amewoo

  ### Using a prediction spread so range of data varies across distributions
  point <- data.frame(x = -90, y = 35)
  d1 <- as_distr(point, bf, crs = "EPSG:4326")
  distr <- predict(bf, d1, season = "prebreeding")
  # Select ~1/3 of the weeks
  sel_col <- seq_len(ncol(distr))[seq_len(ncol(distr)) %% 3 == 0]
  distr <- distr[, sel_col, drop = FALSE]

  # 5. facet with limits
  expect_no_error(p5 <- plot_distr(distr, bf,
                                   show_dynamic_mask = TRUE,
                                   limits = c(0, 0.02)))
  expect_no_error(print(p5))

  # 6. facet with dynamic dynamic scaling (of density values)

  expect_no_error(p6 <- plot_distr(distr, bf,
                                   show_dynamic_mask = TRUE,
                                   dynamic_scale = TRUE))
  expect_no_error(print(p6))
})

test_that(
  "plot_distr() works with custom colors and custom value label and subset", {
  # This test is somewhat of a catch all for remaining minor options
  bf <- BirdFlowModels::amewoo

  expect_no_error(p7 <- plot_distr(get_distr(bf, 1:4), bf,
                                   subset = 2,
                                   gradient_colors = c("red", "yellow", "blue"),
                                   show_dynamic_mask = TRUE,
                                   value_label = "Red-yellow-blue"))

  expect_no_error(print(p7))

})
