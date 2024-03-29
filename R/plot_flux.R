#' Plot bird flux
#'
#' @param flux A flux object as created by
#'   [calc_flux(format = "dataframe")][calc_flux]
#' @param bf A BirdFlow object
#' @param subset A subset of the transitions in `flux` to plot, can be
#' a logical vector of the same length as the number of transitions in `flux`;
#' a numeric index of transitions in `flux`, or a subset of the transition names
#' in `flux`.
#' @param limits Two numbers representing the range in flux values to
#' display. Values outside of this range will be truncated to the range. With
#' the default of `NULL` the entire range is plotted.
#' @param dynamic_scale  If `TRUE` then the range of the data in each
#' transition is mapped to the color palette. This makes it easier to see
#' the variation within a single transition but results in an inconsistent
#' scale among transitions.
#' @param coast_linewidth The line width used to plot the coast. Set to `NULL`
#' to skip plotting the coastline.
#' @param coast_color The color used to plot the coastline, or `NULL` to skip
#' plotting the coastline.
#' @param gradient_colors The colors palette used to represent the flux
#' intensity.
#' @param title The plot title
#' @param value_label The label for the flux values.
#' @return `plot_flux` returns a **ggplot2** object.  It can be displayed with
#' `print()`.
#' @export
#' @inherit calc_flux examples
plot_flux <- function(flux,
                      bf,
                      subset = NULL,
                      limits = NULL,
                      dynamic_scale = FALSE,
                      coast_linewidth =  .25,
                      coast_color = gray(0.5),
                      gradient_colors = NULL,
                      title = species(bf),
                      value_label = "Flux") {

  if (!is.null(limits) && dynamic_scale) {
    stop("Do not set dynamic_scale to TRUE while also setting limits.")
  }

  if (dynamic_scale) {
    distr <- apply(distr, 2, function(x) x / max(x, na.rm = TRUE))
  }


  if (is.null(limits)) {
    limits <- range(flux$flux, na.rm = TRUE)
  } else {
    stopifnot(is.numeric(limits), length(limits) == 2, all(!is.na(limits)),
              limits[1] < limits[2])
    # Truncate to limits

    flux$flux[flux$flux < limits[1]] <- limits[1]
    flux$flux[flux$flux > limits[2]] <- limits[2]
  }


  if (is.null(gradient_colors)) {
    gradient_colors <-
      c("#EDDEA5", "#FCCE25", "#FBA238", "#EE7B51", "#DA596A", "#BF3984",
        "#9D189D", "#7401A8", "#48039F", "#0D0887")
  }

  # Apply subset

  if (!is.null(subset)) {

    transitions <- unique(flux$transition)

    if (is.logical(subset)) {
      if (!length(subset) == length(transitions))
        stop("Logical subset length should match the number of transitions (",
             length(transitions), ")")
      transitions <- transitions[subset]
    } else if (is.numeric(subset)) {
      if (anyNA(subset) ||
         !all.equal(subset, floor(subset)) ||
         any(subset < 1) ||
         any(subset > length(transitions))) {
        stop("Numeric subset should contain only integer values between 1 and ",
             length(transitions), ".")
      }
      transitions <- transitions[subset]
    } else if (is.character(subset)) {
      if (!all(subset %in% names(transitions))) {
        stop("Character subset should contain only transition names",
             " in (flux$transtion).")
      }
      transitions <- transitions[transitions %in% subset]
    }

    flux <- flux[flux$transition %in% transitions, , drop = FALSE]

  }
  transitions <- unique(flux$transition)


  # Start plot
  p <- flux |>
    #dplyr::filter(transition %in% transitions[seq(4, 50, 4)]) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y,
                                 fill = .data$flux)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = gradient_colors)


  # Add facet wrap and title
  if (length(transitions > 1)) {
    # Multiple transitions, facet wrap on date and add species title
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$date)) +
      ggplot2::ggtitle(title)
  } else {
    # Single transition add species title AND date subtitle
    p <- p +
      ggplot2::ggtitle(title, subtitle = flux$date[1])
  }

  # Add coastline
  if (!is.null(coast_color) && !is.null(coast_linewidth)) {

    coast <- get_coastline(bf)

    p  <- p +
      ggplot2::geom_sf(data = coast,
                       inherit.aes = FALSE,
                       linewidth = coast_linewidth,
                       color = coast_color)
  }

  return(p)
}
