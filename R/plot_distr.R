
#' Plot distributions
#'
#' Return a [ggplot2::ggplot] object with plots of one or more
#' distributions.
#'
#' @details
#' ## Masks
#'
#' BirdFlow objects have both a mask and a dynamic
#' mask. For any given timestep the model only covers cells that aren't excluded
#' by either of these masks.
#'
#' * **mask** The static mask, AKA "mask", indicates which cells
#' in the raster extent are included in the model at any timestep these are
#' also called active cells (e.g. [n_active()]). The number of elements in a
#' distribution always match the number of unmasked cells in the (static) mask.
#' Cells are masked if they always have zero abundance in the Bird S&T data
#' for every week of the year.
#'
#' * **dynamic mask** The dynamic mask indicates which of the cells not excluded
#' by the static mask are modeled within each timestep. It,like distributions,
#' only has values for active cells. Thus the dimensions of objects returned
#' by [get_dynamic_mask(bf)](get_dynamic_mask()) and get
#' [get_distr(bf)](get_distr()) will be identical.
#' The purpose of the dynamic mask is to improve efficiency of fitting,
#' storing, and using BirdFlow models by eliminating unlikely locations
#' in the model.  The dynamic mask includes cells that have a zero in the
#' eBird S&T abundance for the associated week.
#'
#' To display both masks set `show_dynamic_mask` to `TRUE` and leave
#' `show_mask` at it's default (`TRUE`).  Showing the dynamic mask relies on
#' matching the columns names of the distribution to timesteps in the model so
#' requires that the column names match the column names in `get_distr()`.
#'
#' ## Subtitles
#' Plot subtitles will be derived from the column names in `distr`. It may be
#' useful to define single distributions as a one column matrix so that the
#' label information is there. For example if `d` is a distribution in a
#' matrix with multiple columns use `drop=FALSE` while subsetting,
#' `plot_distr(d[, 1, drop = FALSE], bf)`; and to add a label to a vector
#' distribution use `d <- matrix(d, ncol = 1);  colnames(d) <- "new label"`
#'
#' @param distr A vector or matrix with distribution values corresponding to
#'   active cells in `bf`.
#' @param bf A BirdFlow object.
#' @param subset Defines an optional subset of `distr` that should be plotted.
#'   Use either column numbers, column names, or a logical vector.
#' @param show_mask If `TRUE` (the default) the static mask that indicates which
#'   cells are active in the model at any timestep will be shown.
#' @param show_dynamic_mask Defaults to `FALSE`. Set to `TRUE` to visualize the
#'   dynamic mask. This is achieved by overwriting cells that are dynamically
#'   masked with NA. For `show_dynamic_mask = TRUE` to work the column names in
#'   `distr` should all be in `colnames(get_distr(bf))`.  This is
#'   true for distributions returned by [`predict()`](predict.BirdFlowR)
#'   and [get_distr()].
#' @param limits The range of density values to map `gradient_colors` to. The
#'   default is the range of the values in `distr` after applying `subset`. If
#'   you want to standardize the range across multiple models of a single
#'   species you might want to set to `c(0, max)` where `max` is the maximum
#'   observed value across all models. Alternatively if the range is highly
#'   variable among the columns in ``distr`` as when density spreads out from a
#'   single point in the results of [`predict(bf)`](predict.BirdFlowR) you may
#'   want to set this smaller than the full range in which case the values will
#'   be truncated to the limits (see examples).
#' @param dynamic_scale Set to `TRUE` to have the range of the data in each
#'   distribution mapped to the full color gradient. This allows visualizing the
#'   full range of values within each timestep optimally at the cost of
#'   consistency in the color scale among the facets - or animation frames
#'   if using [animate_distr()].
#' @param coast_linewidth The line width to use when plotting the coast. Default
#'   is `0.25`.  If `NULL` the coast will not be plotted.
#' @param coast_color The color to use for plotting the coastline. If `NULL` the
#'   coast will not be plotted.
#' @param gradient_colors A color gradient that will be used to plot the density
#'   values. Leave or set to `NULL` for the default of
#'   `ebirdst::abundance_palette(10, season = "weekly")`.
#' @param active_cell_color The background color for active cells in the
#'   landscape. Only used if `show_mask` is `TRUE`. These cells will only be
#'   visible if there are `NA` values in `distr` or if `show_dynamic_mask`
#'   is `TRUE`.
#' @param inactive_cell_color The color to use for inactive cells in the
#'   landscape. These are cells that are always masked. Only relevant when
#'   `show_mask = TRUE`.
#' @param title The title for the plot. It defaults to the common name of the
#'   species (`species(bf)`).
#' @param value_label The label used for the values in the distribution.
#'   Defaults to "Density"
#'
#'
#' @return [ggplot2::ggplot()] object.  Use `print()` to render it.
#' @export
#' @importFrom grDevices gray grey
#' @examples
#' bf <- BirdFlowModels::amewoo
#' p <- plot_distr(get_distr(bf, c(1,11, 21)), bf, show_dynamic_mask = TRUE)
#' @seealso
#' * [animate_distr()] for animating distributions.
#' * [plot_routes()] and  [animate_routes()] for visualizing routes.
#' * [as_distr()], [get_distr()], [`predict()`](predict.BirdFlow)
#' for functions that produce distributions.
plot_distr <- function(distr,
                       bf,
                       subset = NULL,
                       show_mask = TRUE,
                       show_dynamic_mask = FALSE,
                       limits = NULL,
                       dynamic_scale = FALSE,
                       coast_linewidth =  .25,
                       coast_color = gray(0.5),
                       gradient_colors = NULL,
                       active_cell_color = rgb(1, 1, 1, .3),
                       inactive_cell_color = rgb(0, 0, 0, .2),
                       title = species(bf),
                       value_label = "Density") {

  if (dynamic_scale) {
    distr <- apply(distr, 2, function(x) x / max(x, na.rm = TRUE))
  }


  if (!missing(subset)) {
    if (is.null(dim(distr)) || length(dim(distr)) != 2) {
      stop("subset should only be used if distr contains multiple ",
           "distributions")
    }

    # Apply subset
    distr <- distr[, subset, drop = FALSE]

    # If a single distr drop dimensions and add attributes
    if (ncol(distr) == 1) {
      name <- colnames(distr)
      distr <- as.vector(distr)
      attr(distr, "time") <- name
    }
  }


  if (is.null(limits)) {
    limits <- range(distr, na.rm = TRUE)
  } else {
    stopifnot(is.numeric(limits), length(limits) == 2, all(!is.na(limits)),
              limits[1] < limits[2])
    # Truncate to limits
    distr[distr < limits[1]] <- limits[1]
    distr[distr > limits[2]] <- limits[2]
  }

  if (show_dynamic_mask) {
    bf <- add_dynamic_mask(bf)
    dm <- get_dynamic_mask(bf)

    multiple <- is.matrix(distr) || is.array(distr)
    if (multiple) {
      if (!all(colnames(distr) %in% colnames(dm)))
        stop(" Cannot apply dynamic mask. ",
             "Not all column labels in distr match time labels from bf.")
      dm <- dm[, match(colnames(distr), colnames(dm))]
      stopifnot(all(colnames(distr) == colnames(dm)))
      distr[!dm] <- NA
    } else {
      label <- attr(distr, "time")
      if (is.null(label) || !label %in% colnames(dm))
        stop(" Cannot apply dynamic mask. ",
             "distr label is missing or not a time label from bf")

      dm <- dm[, colnames(dm) == label]
      distr[!dm] <- NA
    }
  }

  r <- rasterize_distr(distr, bf, format = "dataframe")
  multiple <- length(unique(r$order)) > 1

  names(r)[names(r) == "value"] <- value_label

  # Add original order (column index) to the data and define a labeller tp
  # convert that to the column names.
  # Note: if you use the column names directly to create the facets then
  # they end up in alphabetical order, not the order in the the distr
  # object.
  if (multiple) {
    order <- data.frame(order = seq_len(ncol(distr)), label = colnames(distr))
    order_to_label <- function(x) {
      order$label[match(as.character(x), as.character(order$order))]
    }
    order_labeller  <-  ggplot2::as_labeller(order_to_label)
  }

  coast <- get_coastline(bf)

  if (is.null(gradient_colors))
    gradient_colors <- ebirdst::abundance_palette(10, season = "weekly")

  p <-
    ggplot2::ggplot(r, ggplot2::aes(x = .data$x,
                                    y = .data$y,
                                    fill = .data[[value_label]])) +
    ggplot2::geom_raster()


  if (dynamic_scale) {
    p <- p +  ggplot2::scale_fill_gradientn(colors = gradient_colors,
                                            na.value = active_cell_color,
                                            limits = limits,
                                            breaks = c(0, 1),
                                            labels = c("Min.", "Max."))
  } else {
    p <- p + ggplot2::scale_fill_gradientn(colors = gradient_colors,
                                           na.value = active_cell_color,
                                           limits = limits)
  }

  if (!is.null(coast_color) && !is.null(coast_linewidth)) {

    p  <- p +
      ggplot2::geom_sf(data = coast,
                       inherit.aes = FALSE,
                       linewidth = coast_linewidth,
                       color = coast_color)

  }

  # coord_sf is required to adjust coordinates while using geom_sf
  # Here we are preventing expanding the extent of the plot.
  # Setting the CRS is only necessary when the coastline isn't plotted because
  # of NULL coast_color or coast_linewidth
  p <- p +
    ggplot2::coord_sf(expand = FALSE, crs = crs(bf)) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())

  if (multiple) {
    p <- p +
      ggplot2::facet_wrap(facets = ggplot2::vars(order),
                          labeller = order_labeller) +
      ggplot2::ggtitle(label = title)

  } else {
    subtitle <- r$label[1]
    p <- p +
      ggplot2::ggtitle(label = title, subtitle = subtitle)

  }

  if (show_mask) {
    # This adds

    # Note you can't have more than one geom_raster layer in a ggplot, but
    # you can add a annotation_raster.  It's a matrix with the colors in the
    # cells - often it's used to overlay images on the plot.

    # I also played with annotate(geom = "raster") and got it to work with
    # single plots but not facets.


    # Make a color matrix for the mask
    mask <- bf$geom$mask
    col_mask <- matrix("", nrow(mask), ncol(mask))
    col_mask[mask] <- active_cell_color
    col_mask[!mask] <- inactive_cell_color

    # Add it to the plot
    p <- p + ggplot2::annotation_raster(col_mask, xmin = xmin(bf),
                               xmax = xmax(bf),
                               ymin = ymin(bf),
                               ymax = ymax(bf))


    # Move the new annotation layer to the first layer so it draws under others
    n_layers <- length(p$layers)
    p$layers <- p$layers[c(n_layers, seq_len(n_layers - 1))]
  }

  return(p)

}
