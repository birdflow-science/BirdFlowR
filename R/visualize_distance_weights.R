#' Visualize the spread kernel used by [calc_dist_weights()]
#'
#' `visualize_distance_weights()` plots the shape of the spread kernel used
#' by [calc_dist_weights()] (and, through it, the `"continuous"` and
#' `"continuous-spherical"` methods of [calc_bmtr()]) for one or more
#' transition-line lengths. It's meant as a tool for choosing `kernel` and
#' its hyperparameters (`gamma`, `kl`, `s1`) before running an expensive
#' `calc_bmtr()` call.
#'
#' @param line_lengths A vector of transition-line lengths (m) to visualize
#' the spread kernel for.
#' @param radius_m The detection radius (m). Defaults to `res_m / 2` (the
#' same default used by [calc_bmtr()]'s continuous methods). For
#' `type = "envelope"` this is only shown as a reference line, and only if
#' explicitly supplied. For `type = "raster"` it's used, as in
#' [calc_dist_weights()], to determine the band of probability density
#' that forms the plotted weight.
#' @param res_m The resolution of the associated bird flow model (m), used
#' to determine the nugget added to the variance. See [calc_dist_weights()].
#' @inheritParams calc_dist_weights
#' @param type The style of plot:
#' \describe{
#' \item{`"envelope"`}{(default) For each line length, plots the region
#' within 1.96 standard deviations of the line (in the kernel's probability
#' distribution) as a band along the line. This represents 95 percent of
#' the density.}
#' \item{`"raster"`}{For each line length, plots the actual weight returned
#' by [calc_dist_weights()] over a grid of points around the line, as a
#' heatmap. Slower, but reflects `radius_m`'s effect on the weight as well
#' as the kernel's spread.}
#' }
#' @param n The number of grid points used along (and, for `type = "raster"`,
#' across) each line.
#'
#' @return A `ggplot2` object. It can be displayed with `print()`.
#' @seealso [calc_dist_weights()], [calc_bmtr()]
#' @export
#'
#' @examples
#' visualize_distance_weights()
#' visualize_distance_weights(type = "raster", kernel = "bb", s1 = 20)
visualize_distance_weights <- function(line_lengths = c(2000, 1000, 500, 200),
                                       radius_m = NULL,
                                       res_m = 1000,
                                       kernel = c("m3", "bb", "m1", "m5", "sq"),
                                       gamma = 3e10, kl = 9e5, s1 = 200,
                                       type = c("envelope", "raster"),
                                       n = 100) {
  kernel <- match.arg(kernel)
  type <- match.arg(type)

  radius_supplied <- !is.null(radius_m)
  if (!radius_supplied) {
    radius_m <- res_m / 2
  }

  kernel_names <- c(m1 = "Matern 1/2", m3 = "Matern 3/2", m5 = "Matern 5/2",
                    sq = "Squared exponential", bb = "Brownian bridge")
  hp_text <- if (kernel == "bb") {
    paste0("s1 = ", s1)
  } else {
    paste0("gamma = ", gamma, ", kl = ", kl)
  }
  plot_title <- paste0("Spread kernel: ", kernel_names[[kernel]])
  plot_subtitle <- paste0("kernel = \"", kernel, "\" (", hp_text, ")")

  line_lengths <- sort(line_lengths, decreasing = TRUE)
  length_levels <- paste(line_lengths, "m")
  length_label <- factor(length_levels, levels = length_levels)

  if (type == "envelope") {
    bands <- lapply(seq_along(line_lengths), function(i) {
      len <- line_lengths[i]
      dist_along <- seq(0, len, length.out = n)
      sd_km <- calc_dist_weights_sd(dist_along, rep(len, n), res_m,
                                    kernel = kernel, gamma = gamma, kl = kl,
                                    s1 = s1) / 1000
      data.frame(dist_along_km = dist_along / 1000,
                ymin = -1.96 * sd_km, ymax = 1.96 * sd_km,
                length_label = length_label[i])
    })
    df <- do.call(rbind, bands)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$dist_along_km,
                                          ymin = .data$ymin,
                                          ymax = .data$ymax,
                                          fill = .data$length_label)) +
      ggplot2::geom_ribbon(alpha = 0.5) +
      ggplot2::scale_fill_viridis_d(name = "Line length")

    if (radius_supplied) {
      p <- p + ggplot2::geom_hline(yintercept = c(-radius_m, radius_m) / 1000,
                                   linetype = "dashed")
    }
  } else {
    # Raster
    max_sd_m <- vapply(line_lengths, function(len) {
      dist_along <- seq(0, len, length.out = n)
      max(calc_dist_weights_sd(dist_along, rep(len, n), res_m,
                               kernel = kernel, gamma = gamma, kl = kl,
                               s1 = s1))
    }, numeric(1))
    y_max_m <- 3 * max(max_sd_m)
    dist_to_seq <- seq(-y_max_m, y_max_m, length.out = n)

    dist_to_width_km <- diff(dist_to_seq)[1] / 1000

    grids <- lapply(seq_along(line_lengths), function(i) {
      len <- line_lengths[i]
      dist_along <- seq(0, len, length.out = n)
      grid <- expand.grid(dist_along_line = dist_along,
                          dist_to_line = dist_to_seq)
      grid$weight <- calc_dist_weights(
        dist_to_line = abs(grid$dist_to_line),
        dist_along_line = grid$dist_along_line,
        line_lengths = len, radius_m = radius_m, res_m = res_m,
        kernel = kernel, gamma = gamma, kl = kl, s1 = s1)
      grid$length_label <- length_label[i]
      grid$dist_along_width_km <- (len / (n - 1)) / 1000
      grid
    })
    df <- do.call(rbind, grids)
    df$dist_along_km <- df$dist_along_line / 1000
    df$dist_to_km <- df$dist_to_line / 1000

    # geom_tile() (rather than geom_raster()) since the along-line grid
    # spacing differs between facets (each spans a different line length).
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$dist_along_km,
                                          y = .data$dist_to_km,
                                          width = .data$dist_along_width_km,
                                          height = dist_to_width_km,
                                          fill = .data$weight)) +
      ggplot2::geom_tile() +
      ggplot2::facet_wrap(~length_label) +
      ggplot2::scale_fill_viridis_c(name = "Weight")
  }

  p +
    ggplot2::labs(x = "Distance along line (km)",
                 y = "Perpendicular distance from line (km)",
                 title = plot_title, subtitle = plot_subtitle) +
    ggplot2::coord_fixed() +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_width(res_m / 1000 * 2)) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(res_m / 1000 * 2))
}
