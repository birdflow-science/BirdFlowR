


#' Plot distributions with ggplot2
#'
#' Return a [ggplot2::ggplot()] based on one or more distributions.
#'
#' @param distr a vector or matrix with distribution values corresponding to
#' active cells in `bf`.
#' @param bf A BirdFlow object
#' @param subset Indicates a subset of distr should be plotted. Use either
#' column numbers, column names, or a logical vector.
#' @param density_colors A color gradient that will be used to plot the density
#' values.  Leave or set to `NULL` for the default of
#' `ebirdst::abundance_palette(10, season = "weekly")`.
#' @param coast_linewidth The linewidth to use when plotting the coast. Default
#' is 0.25.  If NULL the coast will not be plotted.
#' @param coast_color The color to use for plotting the coastline. If NULL the
#' coast will not be plotted.
#' @param inactive_color The color to use for inactive cells in the landscape.
#' @param limits The range of density values to map `density_colors` to. The
#' default is the range of the values in distr (prior to applying `subset`).
#' If you want to standardize the range across multiple models of a single
#' species you might want to set to `c(0, max)` where `max` is the maximum
#' observed value across all models.
#' @param title The title for the plot. It defaults to the common name of the
#' species.
#'
#' @return A [ggplot2::ggplot()] object.  Use `print()` to render it.
#' @export
#'
#' @examples
#' plot_distr(get_distr(bf, 1), bf)
#' @seealso
#' * [plot_route()] for plotting routes generated from a BirdFlow object.
#' * [rasterize_distr(distr, bf, format = "dataframe")](rasterize_distr())
#' for converting a distribution to a dataframe with raster data for use with
#'  [ggplot2::geom_raster()]
plot_distr <- function(distr, bf, subset = NULL,
                       density_colors = NULL,
                       coast_linewidth =  .25,
                       coast_color = grey(0.5),
                       inactive_color =  rgb(.9, .9, .9, .5),
                       limits = NULL,
                       title = species(bf),
                       relative_density = FALSE){

  if(relative_density){
    distr <- apply(distr, 2, function(x) x / max(x, na.rm = TRUE) )
  }

  if(is.null(limits))
    limits <- range(distr, na.rm = TRUE)


  if(!missing(subset)){
    if(is.null(dim(distr)) || length(dim(distr)) != 2 ){
      stop("subset should only be used if distr contains multiple distributions")
    }

    # Apply subset
    distr <- distr[, subset, drop = FALSE]

    # If a single distr drop dimensions and add attribute
    if(ncol(distr) == 1){
      name <- colnames(distr)
      distr <- as.vector(distr)
      attr(distr, "time")<- name
    }
  }


  r <- rasterize_distr(distr, bf, format = "dataframe" )
  multiple <- length(unique(r$time)) > 1

  density_label <- ifelse(relative_density, "Relative Density", "Density")
  names(r)[names(r) == "density"] <- density_label

  # Ad original order (column index) to the data
  if(multiple){
    order <- data.frame(order = 1:ncol(distr), label = colnames(distr))
    r$order <- order$order[match(r$time, order$label)]
    order_to_label <- function(x) {
      order$label[match(as.character(x), as.character(order$order))]}

    order_labeller  <-  as_labeller(order_to_label)
  }

  coast <- get_coastline(bf)



  if(is.null(density_colors))
    density_colors <- ebirdst::abundance_palette(10, season = "weekly")


  multiple <- length(unique(r$time)) > 1

  p <-
    ggplot(r, aes(x = x, y = y, fill = .data[[density_label]])) +
    geom_raster()


  if(relative_density){
   p <- p +  scale_fill_gradientn(colors = gradient_colors,
                         na.value = inactive_color,
                         limits = limits,
                         breaks = c(0, 1),
                         labels = c("Min.", "Max."))
  } else {
    p <- p + scale_fill_gradientn(colors = gradient_colors,
                         na.value = inactive_color,
                         limits = limits)
  }

  if(!is.null(coast_color) && !is.null(coast_linewidth)){

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

  if(multiple) {
    p <- p +
      ggplot2::facet_wrap(facets = vars(order), labeller = order_labeller ) +
      ggplot2::ggtitle( label = title )

  } else {
    subtitle <- attr(distr, "time")
      p <- p +
        ggplot2::ggtitle(label = title, subtitle = subtitle)

  }
  return(p)
}
