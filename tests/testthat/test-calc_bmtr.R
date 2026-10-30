
test_that("calc_bmtr() works and three methods produce similar output", {

  local_quiet()
  bf <- small_test_bf()

  # Binary
  expect_no_error(binary_bmtr <- calc_bmtr(bf, method = "binary"))
  binary_top <- head(binary_bmtr[!binary_bmtr$bmtr == 0, ], 6)
  binary_top$bmtr <- signif(binary_top$bmtr, 4)
  expect_snapshot(binary_top)

  # continuous
  expect_no_error(continuous_bmtr <- calc_bmtr(bf, method = "continuous"))
  continuous_top <- head(continuous_bmtr[!continuous_bmtr$bmtr == 0, ], 6)
  continuous_top$bmtr <- signif(continuous_top$bmtr, 4)
  expect_snapshot(continuous_top)

  # continuous-spherical
  expect_no_error(spherical_bmtr <- calc_bmtr(bf, method = "continuous-spherical"))
  spherical_top <- head(spherical_bmtr[!spherical_bmtr$bmtr == 0, ], 6)
  spherical_top$bmtr <- signif(spherical_top$bmtr, 4)
  expect_snapshot(spherical_top)


  expect_no_error(plot_bmtr(binary_bmtr, bf))
  expect_no_error(animate_bmtr(continuous_bmtr, bf))


  # Check that Radius is respected - kept within same test block to mimimize calls / testing time
  r1 <- mean(res(bf)) * 0.5  # default used above
  r2 <-  mean(res(bf)) * 0.9

  r2_bmtr <- calc_bmtr(bf, method = "continuous", radius = r2)
  expect_false(isTRUE(all.equal(continuous_bmtr$bmtr, r2_bmtr$bmtr)))


  # Test that euclidean (continuous) and spherical (continous-spherical)
  # agree closely
  euc <- continuous_bmtr
  sph <- spherical_bmtr

  euc$id <- paste(euc$x, euc$y, euc$transition)
  sph$id <- paste(sph$x, sph$y, sph$transition)
  m <- merge(euc, sph, by = "id", suffixes = c("_euc", "_sph"))

  expect_true(nrow(m) > 0)
  expect_equal(m$bmtr_euc, m$bmtr_sph, tolerance = 0.05)

  # calc_bmtr()  forwards ... to the spread kernel
  wider <- calc_bmtr(bf, method = "continuous", gamma = 100000)
  expect_false(isTRUE(all.equal(continuous_bmtr$bmtr, wider$bmtr)))

  # Specifying kernel parameters with binary method produces error
  expect_error(calc_bmtr(bf, method = "binary", gamma = 100000))

  # plot_bmtr() subset drops excluded transitions (no empty facets)
  all_trans <- sort(unique(continuous_bmtr$transition))
  expect_true(length(all_trans) >= 3)

  # Numeric subset: keep first 2 transitions
  expect_no_error(p <- plot_bmtr(continuous_bmtr, bf, subset = 1:2))
  expect_equal(nlevels(p$data$label), 2L)

  # Character subset: keep same 2 transitions by name
  expect_no_error(p2 <- plot_bmtr(continuous_bmtr, bf, subset = all_trans[1:2]))
  expect_equal(nlevels(p2$data$label), 2L)

})

test_that("calc_bmtr() errors on an invalid method", {
  local_quiet()
  bf <- small_test_bf()

  expect_error(calc_bmtr(bf, method = "bogus"))
})

test_that("calc_bmtr() respects a user-supplied points argument (bug fix)", {
  local_quiet()
  bf <- small_test_bf()

  pts <- i_to_xy(seq_len(min(5, n_active(bf))), bf)[, c("x", "y")]

  f <- calc_bmtr(bf, points = pts, method = "continuous", format = "points")
  expect_equal(nrow(f$point), nrow(pts))
  expect_equal(sort(f$point$x), sort(pts$x))
  expect_equal(sort(f$point$y), sort(pts$y))

})


test_that("calc_bmtr() enforces check_radius bounds for continuous methods", {
  local_quiet()
  bf <- small_test_bf()

  bad_radius <- mean(res(bf)) * 2

  for (method in c("continuous")) {
    expect_error(calc_bmtr(bf, method = method, radius = bad_radius))
    expect_no_error(calc_bmtr(bf, method = method, radius = bad_radius,
                              check_radius = FALSE))
  }
})


test_that("Test sensativity of bmtr to radius", {
  skip_if_not_installed("BirdFlowExamples")
  local_quiet()
  testthat::skip("In depth bmtr radius analysis - always skipped")

  # This is an exploration of how bmtr varies across a very wide range
  # of values for the radius parameter.
  # It's a "test" only in the sense that it improves understanding of the
  # functions behavior, but this seemed as good a place to save it as any.
  # Conclusions are at bottom.

  # Sparsify and truncate to speed things up
  bf <- BirdFlowExamples::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 5)
  bf <- sparsify(bf, "conditional", .9, p_protected = 0.05)

  # Set radii to calculate bmtrs for
  rads <- mean(res(bf)) / 2 * c(0.1, 0.25, 0.5, 1, 4, 8, 16, 32, 64)

  # Calculate bmtrs
  bmtrs <- vector(mode = "list", length(rads))
  for (i in seq_along(rads)) {
    bmtrs[[i]] <- calc_bmtr(bf, radius = rads[i], check_radius = FALSE)
  }


  # Plot BMTR
  plot_dir <- tempdir()
  files <- file.path(plot_dir,
                     paste0("bmtr_", rads * 2 / 1000, ".png"))
  for (i in seq_along(rads)) {
    ragg::agg_png(filename = files[i], width = 6, height = 6,
                  res = 150, units = "in")
    plot_bmtr(bmtrs[[i]], bf,
              title = paste0("Radius: ", rads[i] / 1000, " km")) |>
      print()

    dev.off()
  }

  # Assess how total bmtr changes with radius


  # Total bmtr
  # With larger radius more response points are added
  tf <- sapply(bmtrs, function(x) sum(x$bmtr))

  # Create uniform set of points by filtering to just the points that are
  # in the smallest radius bmtr
  add_id <- function(x) {
    x$id <- paste0(x$x, "-", x$y)
    x
  }
  bmtrs <- lapply(bmtrs, add_id)
  ids <- unique(bmtrs[[1]]$id)
  bmtrs2 <- lapply(bmtrs, function(x) x[x$id %in% ids, , drop = FALSE])

  # total bmtr by radius with fixed points
  tf2 <- sapply(bmtrs2, function(x) sum(x$bmtr))

  # Prep for plotting
  df <- data.frame(radius = rads, expanding = tf, fixed = tf2)
  ldf <- tidyr::pivot_longer(df, cols = c(expanding, fixed),
                             values_to = "bmtr",
                             names_to = "points")
  ldf$diameter <- 2 * ldf$radius / 1000  # diameter in km
  dim <- mean(res(bf)) / 1000
  dim_col <- rgb(0, 0, 0, .5)
  e <- ext(bf) / 1000
  width <- e[2] - e[1]
  height <- e[4] - e[3]

  # Make plot of how the total bmtr changes with diameter
  p <- ggplot2::ggplot(data = ldf, ggplot2::aes(x = .data$diameter,
                                  y = .data$bmtr,
                                  color = .data$points)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = c(dim, width, height), color = dim_col) +
    ggplot2::geom_text(ggplot2::aes(x = dim,
                                    label = paste0("\nCell dimension (",
                                                   round(dim),
                                                   " km)"),
                                    y = 0.035),
                       color = dim_col, angle = 90) +
    ggplot2::geom_text(ggplot2::aes(x = height,
                                    label = paste0("\nExtent height (",
                                                   round(height),
                                                   " km)"),
                                    y = 0.043),
                       color = dim_col, angle = 90) +
    ggplot2::geom_text(ggplot2::aes(x = width,
                                    label = paste0("\nExtent width (",
                                                   round(width),
                                                   " km)"),
                                    y = 0.044),
                       color = dim_col, angle = 90) +
    ggplot2::ggtitle("Total BMTR") +
    ggplot2::scale_x_log10()

  ragg::agg_png(filename = file.path(plot_dir, "total_bmtr_diam.png"),
      width = 6, height = 6, res = 150, units = "in")

  print(p)
  dev.off()

  # I think there are a few things going on here:
  # 1. As radius approaches zero the bmtr increases. I suspect this is because
  #  the response points are on the same grid as the starting and ending
  #  locations.  As the radius approaches zero you multiply the bmtr by a number
  #  that approaches infinity but our points are positioned such that the
  #  bmtr value doesn't fall off as fast as we would expect given say, random
  # points.
  # 2. As radius increases substantially beyond the cell size than you can pick
  #    up points that are beyond the movement line.  This means it begins
  #   to act more like a squared function than a linear function.
  #   At extremes if the radius is much larger than the movement distances
  #   than the lines we are intersecting begin to look a lot more like points
  #   and the relationship between radius and abundance that intersects the
  #   circle approaches the square of the radius.
  # 3. (2) begins to break down as the diameter approaches the extent width,
  #   at that point you've already captured most of the movement at every point
  #   and a larger radius just means you divide that total by a bigger number.
  #
  #  Two conclusions:
  #    1. as implemented a diameter equal to the cell size is a sweet spot, and
  #    keeping the diameter within the range between > 0.5 and < 2 cell widths
  #    is a good idea (and radius half that).
  #    2. If we want the algorithm to work with larger diameters it would make
  #    sense to not allow points beyond the end of the line to be counted
  #    (even if they are within the radius of the line).  I suspect this
  #    would slow things down unless I implemented it in C++ as I don't think
  #    I can do it with any kind of vectorization in R.

})
