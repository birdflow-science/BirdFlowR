#' Plot BirdFlow Migration Traffic Rate (BMTR)
#'
#' @param bmtr A data frame created by
#'   [calc_bmtr(format = "dataframe")][calc_bmtr]
#' @param bf A BirdFlow object
#' @param subset A subset of the transitions in `bmrt` to plot, can be
#' a logical vector of the same length as the number of transitions in `bmtr`;
#' a numeric index of transitions in `bmtr`, or a subset of the transition names
#' in `bmtr`.
#' @param limits Two numbers representing the range in bmtr values to
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
#' @param gradient_colors The colors palette used to represent the BMTR
#' intensity.
#' @param title The plot title
#' @param value_label The label for the BMTR values.
#' @param transform A transformation to apply to the color scaling.
#' `"identity"`, and `"sqrt"` are recommended.
#' If `"log"` is used zeros will be replaced with
#' 1/2 the smallest non-zero value prior to transforming.
#' Legend will still reflect the original values.
#' Passed to [ggplot2::scale_color_gradient()].
#' @return `plot_bmtr` returns a **ggplot2** object.  It can be displayed with
#' `print()`.
#' @export
#' @inherit calc_bmtr examples
plot_bmtr <- function(bmtr,
                      bf,
                      subset = NULL,
                      limits = NULL,
                      dynamic_scale = FALSE,
                      coast_linewidth =  .25,
                      coast_color = gray(0.5),
                      gradient_colors = NULL,
                      title = species(bf),
                      value_label = "BMTR",
                      transform = "identity") {

  if (!is.null(limits) && dynamic_scale) {
    stop("Do not set dynamic_scale to TRUE while also setting limits.")
  }

  if("flux" %in% names(bmtr))
    names(bmtr)[names(bmtr) == "flux"] <- "bmtr" ### back compatibility

  if (dynamic_scale) {

    # Scale each transition 0 to 1
    for (t in unique(bmtr$transition)) {
      sv <- bmtr$transition == t
      bmtr$bmtr[sv] <- range_rescale(bmtr$bmtr[sv])
    }

  }

  if (transform == "log") {
    min_non_zero <- min(bmtr$bmtr[!bmtr$bmtr == 0], na.rm = TRUE)
    if (min_non_zero < 0)
      stop("Can't log transflorm bmtr with negative values.")

    bmtr$bmtr[bmtr$bmtr == 0] <- min_non_zero / 2
  }

  # Add "<Month> <mday>" labels as ordered factor
  dates <- lubridate::as_date(bmtr$date)
  label <- paste0(lubridate::month(dates, label = TRUE, abbr = FALSE), " ",
                  lubridate::mday(dates))
  ud <- sort(unique(dates))
  ul <- paste0(lubridate::month(ud, label = TRUE, abbr = FALSE), " ",
               lubridate::mday(ud))
  label <- ordered(label, levels = ul)
  bmtr$label <- label

  if (is.null(limits)) {
    limits <- range(bmtr$bmtr, na.rm = TRUE)
  } else {
    stopifnot(is.numeric(limits), length(limits) == 2, all(!is.na(limits)),
              limits[1] < limits[2])
    # Truncate to limits

    bmtr$bmtr[bmtr$bmtr < limits[1]] <- limits[1]
    bmtr$bmtr[bmtr$bmtr > limits[2]] <- limits[2]
  }


  if (is.null(gradient_colors)) {
    gradient_colors <-
      c("#EDDEA5", "#FCCE25", "#FBA238", "#EE7B51", "#DA596A", "#BF3984",
        "#9D189D", "#7401A8", "#48039F", "#0D0887")
  }

  # Apply subset

  if (!is.null(subset)) {

    transitions <- unique(bmtr$transition)

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
             " in (bmtr$transtion).")
      }
      transitions <- transitions[transitions %in% subset]
    }

    bmtr <- bmtr[bmtr$transition %in% transitions, , drop = FALSE]

  }
  transitions <- unique(bmtr$transition)


  # Start plot
  p <- bmtr |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y,
                                 fill = .data$bmtr)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = gradient_colors,
                                  name = value_label,
                                  transform = transform)


  # Add facet wrap and title
  if (length(transitions > 1)) {
    # Multiple transitions, facet wrap on date and add species title
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$label)) +
      ggplot2::ggtitle(title)
  } else {
    # Single transition add species title AND date subtitle
    p <- p +
      ggplot2::ggtitle(title, subtitle = bmtr$label[1])
  }



  # Add coastline
  if (!is.null(coast_color) && !is.null(coast_linewidth)) {

    suppress_specific_warnings({
      coast <- get_coastline(bf)
    }, "No objects within extent. Returning empty sf object.")

    if (nrow(coast) > 0) {
      p  <- p +
        ggplot2::geom_sf(data = coast,
                         inherit.aes = FALSE,
                         linewidth = coast_linewidth,
                         color = coast_color)
    }
  }
  # coord_sf is required to adjust coordinates while using geom_sf
  # Here we are preventing expanding the extent of the plot.
  # Setting the CRS is only necessary when the coastline isn't plotted because
  # of NULL coast_color or coast_linewidth
  p <- p +
    ggplot2::coord_sf(expand = FALSE, crs = crs(bf)) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())

  return(p)
}




#' Plot Bird Flow Migration Traffic Rate (BMTR)
#'
#' DEPRECATED FUNCTION.  Please use [plot_bmtr()] instead.
#' @inheritDotParams plot_bmtr -bmtr
#' @param flux the output from [calc_bmtr()] or, deprecated, [calc_flux()]
#' @inherit plot_bmtr return
#' @export
plot_flux <- function(flux, ...){
  warning("plot_flux() is deprecated. ",
          "Please use plot_bmtr() instead.")
  plot_bmtr(flux, ...)

}

