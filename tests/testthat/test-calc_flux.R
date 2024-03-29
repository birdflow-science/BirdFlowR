test_that("calc_flux() works without directionality", {

  # Sparsify and truncate to speed things up
  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 5)
  bf <- sparsify(bf, "conditional", .9, p_protected = 0.05)

  expect_no_error(f <- calc_flux(bf))

  # Snapshot of first 6 non-zero movements
  top <- head(f[!f$flux == 0, ], 6)
  top$flux <- signif(top$flux, 4)
  expect_snapshot(top)

  # Visualizations
  expect_no_error(plot_flux(f, bf))
  expect_no_error(animate_flux(f, bf))

})


test_that("Test sensativity of flux to radius", {

  testthat::skip("In depth flux radius analysis - always skipped")

  # Sparsify and truncate to speed things up
  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 5)
  bf <- sparsify(bf, "conditional", .9, p_protected = 0.05)

  # Set radii to calculate fluxes for
  rads <- mean(res(bf)) / 2 * c(0.1, 0.25, 0.5, 1, 4, 8, 16, 32, 64)

  # Calculate fluxes
  fluxes <- vector(mode = "list", length(rads))
  for (i in seq_along(rads)) {
    fluxes[[i]] <- calc_flux(bf, radius = rads[i], check_radius = FALSE)
  }


  # Plot fluxes
  plot_dir <- tempdir()
  files <- file.path(plot_dir,
                     paste0("flux_", rads * 2 / 1000, ".png"))
  for (i in seq_along(rads)) {
    ragg::agg_png(filename = files[i], width = 6, height = 6,
                  res = 150, units = "in")
    plot_flux(fluxes[[i]], bf,
              title = paste0("Radius: ", rads[i] / 1000, " km")) |>
      print()

    dev.off()
  }

  # Assess how total flux changes with radius


  # Total flux
  # With larger radius more response points are added
  tf <- sapply(fluxes, function(x) sum(x$flux))

  # Create uniform set of points by filtering to just the points that are
  # in the smallest radius flux
  add_id <- function(x) {
    x$id <- paste0(x$x, "-", x$y)
    x
  }
  fluxes <- lapply(fluxes, add_id)
  ids <- unique(fluxes[[1]]$id)
  fluxes2 <- lapply(fluxes, function(x) x[x$id %in% ids, , drop = FALSE])

  # total flux by radius with fixed points
  tf2 <- sapply(fluxes2, function(x) sum(x$flux))

  # Prep for plotting
  df <- data.frame(radius = rads, expanding = tf, fixed = tf2)
  ldf <- tidyr::pivot_longer(df, cols = c(expanding, fixed),
                             values_to = "flux",
                             names_to = "points")
  ldf$diameter <- 2 * ldf$radius / 1000  # diameter in km
  dim <- mean(res(bf)) / 1000
  dim_col <- rgb(0, 0, 0, .5)
  e <- ext(bf) / 1000
  width <- e[2] - e[1]
  height <- e[4] - e[3]

  # Make plot of how the total flux changes with diameter
  p <- ggplot2::ggplot(data = ldf, ggplot2::aes(x = .data$diameter,
                                  y = .data$flux,
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
    ggplot2::ggtitle("Total Flux") +
    ggplot2::scale_x_log10()

  ragg::agg_png(filename = file.path(plot_dir, "total_flux_diam.png"),
      width = 6, height = 6, res = 150, units = "in")

  print(p)
  dev.off()

  # I think there are a few things going on here:
  # 1. As radius approaches zero the flux increases. I suspect this is because
  #  the response points are on the same grid as the starting and ending
  #  locations.  As the radius approaches zero you multiply the flux by a number
  #  that approaches infinity but our points are positioned such that the
  #  flux value doesn't fall off as fast as we would expect given say, random
  # points.
  # 2. As radius increases substantially beyond the cell size than you can pick
  #    up points that are beyond the movement line.  This means it begins
  #   to act more like a squared function than a linear function.
  #   At extremes if the radius is much larger than the movement distances
  #   than the lines we are intersecting begin to look a lot more like points
  #   and the relationship between radius and abundance that intersects the
  #   circle approaches the square of the radius.
  # 3. (2) begins to break down as the diameter approaches the extent width,
  #   at that point you've already captured most of the movent at every point
  #   and a larger radius just means you divide that total by a bigger number.
  #
  #  Two conclusions:
  #    1. as implemented a diameter equal to the cell size is a sweet spot, and
  #    keeping the diameter within the range between > 0.5 and < 2 cell widths
  #    is a good idea.
  #    2. If we want the algorithm to work with larger diameters it would make
  #    sense to not allow points beyond the end of the line to be counted
  #    (even if they are within the radius of the line).  I suspect this
  #    would slow things down unless I implemented it in C++ as I don't think
  #    I can do it with any kind of vectorization in R.




})
