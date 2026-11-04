test_that("plot_distr() works with default parameters", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo

  ## Default parameters

  # 1. single plot
  expect_no_error(p1 <- plot_distr(get_distr(bf, 1), bf))
  expect_no_error(print(p1))

  # 2 facet
  expect_no_error(p2 <- plot_distr(get_distr(bf, 1:3), bf))
  expect_no_error(print(p2))
})

test_that("plot_distr() works with dynamic mask", {
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo

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
  skip_if_not_installed("BirdFlowExamples")
  bf <- BirdFlowExamples::amewoo

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
  skip_if_not_installed("BirdFlowExamples")
  # This test is somewhat of a catch all for remaining minor options
  bf <- BirdFlowExamples::amewoo

  expect_no_error(p7 <- plot_distr(get_distr(bf, 1:4), bf,
                                   subset = 2,
                                   gradient_colors = c("red", "yellow", "blue"),
                                   show_dynamic_mask = TRUE,
                                   value_label = "Red-yellow-blue"))

  expect_no_error(print(p7))

})

test_that("plot_distr() works with eBird coverage overlay", {
  bf <- small_test_bf()

  # 8. single plot
  expect_no_error(p8 <- plot_distr(get_distr(bf, 1), bf,
                                   show_ebird_coverage = TRUE))
  expect_no_error(print(p8))

  # 9. facet
  expect_no_error(p9 <- plot_distr(get_distr(bf, 1:3), bf,
                                   show_ebird_coverage = TRUE))
  expect_no_error(print(p9))

  # 10. combined with dynamic mask
  expect_no_error(p10 <- plot_distr(get_distr(bf, 1:3), bf,
                                    show_ebird_coverage = TRUE,
                                    show_dynamic_mask = TRUE))
  expect_no_error(print(p10))

  # 11. custom no_coverage_color
  expect_no_error(p11 <- plot_distr(get_distr(bf, 1), bf,
                                    show_ebird_coverage = TRUE,
                                    no_coverage_color = "orange"))
  expect_no_error(print(p11))
})

test_that("plot_distr() eBird coverage overlay errors informatively", {
  bf <- small_test_bf()

  # Model without coverage metadata
  bf_no_cov <- bf
  bf_no_cov$metadata$ebird_coverage <- NA
  expect_error(plot_distr(get_distr(bf_no_cov, 1), bf_no_cov,
                          show_ebird_coverage = TRUE),
              "does not have ebird_coverage metadata")

  # Column labels that don't match model timesteps (matrix form)
  d <- get_distr(bf, 1)
  d <- matrix(d, ncol = 1)
  colnames(d) <- "not a real timestep"
  expect_error(plot_distr(d, bf, show_ebird_coverage = TRUE),
              "Not all column labels")

  # Missing time label (vector form)
  d_novec <- as.vector(get_distr(bf, 1))
  expect_error(plot_distr(d_novec, bf, show_ebird_coverage = TRUE),
              "distr label is missing")
})

test_that("plot_distr() eBird coverage overlay includes inactive cells", {
  bf <- small_test_bf()
  d1 <- get_distr(bf, 1)
  p <- plot_distr(d1, bf, show_ebird_coverage = TRUE)

  tile_layer <- Filter(function(l) inherits(l$geom, "GeomTile"), p$layers)
  expect_length(tile_layer, 1)
  overlay_data <- tile_layer[[1]]$data

  cov_df <- get_ebird_coverage(bf, format = "dataframe")
  t1 <- cov_df[cov_df$label == attr(d1, "time"), ]
  expected <- t1[!t1$coverage, c("x", "y")]

  # Overlay should match exactly the uncovered cells, active or not, since
  # the overlay is meant to distinguish "predicted zero" from "not modeled"
  expect_equal(nrow(overlay_data), nrow(expected))
  expect_setequal(paste(overlay_data$x, overlay_data$y),
                  paste(expected$x, expected$y))
  expect_true(any(is.na(t1$i[!t1$coverage])))  # sanity: some are inactive
})
