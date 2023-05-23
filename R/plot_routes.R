
#' Plot routes
#'
#' Plot routes as lines with color indicating the passage of time and dot size
#' indicating the length of stay at each stop.
#'
#' @param routes The output of [route()] or
#' [route_migration()].  The `$point` component of such an object will also
#' work.
#' @param bf A BirdFlow object
#' @param facet if TRUE then use [ggplot2::facet_wrap()] to show each route
#' out into a separate subplot.
#' @param max_stay_len Used to scale the stay length dots. If NULL
#' (the default) it will be set to the maximum "stay_len" value in `routes`.
#' Set it manually to keep the dot scaling consistent across multiple plots.
#' @return a ggplot object. Use [print()] to display it.
#' @export
#' @importFrom rlang .data
#' @examples
#'bf <- BirdFlowModels::amewoo
#'n_spring <- n_fall <- 4
#'spring_rts <- route_migration(bf, n_spring)
#'
#'# All routes together
#'plot_routes(spring_rts, bf)
#'
#'# One panel per route
#'plot_routes(spring_rts, bf, facet = TRUE)
#'
#' # Both spring and fall migrations on same plot
#' fall_rts <- route_migration(bf, n_fall, "fall")
#' fall_rts$points$route <- fall_rts$points$route + n_spring # for unique routes
#' plot_routes(rbind(spring_rts$points, fall_rts$points), bf)
plot_routes <- function(routes, bf, facet = FALSE, max_stay_len = NULL) {

  #----------------------------------------------------------------------------#
  # Data reformatting and preparation
  #----------------------------------------------------------------------------#

  # Check for full output from route() and select just point component
  if (is.list(routes) && all(names(routes) == c("points", "lines")) &&
                            is.data.frame(routes$points)) {
    routes <- routes$points
  }

  # Reformat and add columns to routes
  # pyear is the proportion of the year using it instead of
  # date allows assigning a cyclical color scale

  routes$date <- lubridate::as_date(routes$date)
  routes$pyear <- proportion_of_year(routes$date)

  # Elapsed time at location - used by animate_routes()
  routes <- routes |>
    dplyr::group_by(.data$route, .data$stay_id) |>
    dplyr::mutate(elapsed_stay = dplyr::row_number()) |>
    as.data.frame()


  # The stops, unique locations where they stayed more than a week
  the_stops <- routes[routes$stay_len > 1, ]
  the_stops <- the_stops[!duplicated(the_stops[, c("route", "stay_id")]), ]

  # Make raster showing which cells are active in the model
  rast <- rasterize_distr(get_distr(bf, 1), bf, format = "dataframe")
  rast$value <- !is.na(rast$density)

  # Coastline for this model
  coast <- get_coastline(bf)

  #----------------------------------------------------------------------------#
  # Data summary
  #----------------------------------------------------------------------------#

  # Maximum length of stay
  if (is.null(max_stay_len))
    max_stay_len <- max(routes$elapsed_stay)
  # must be at least 3 so range isn't a point
  max_stay_len <- max(max_stay_len, 3)

  # Set breaks for stay length (label values in legend)
  stay_len_range <- c(1, max_stay_len) # range of stay length
  stay_len_breaks <- unique(sort(c(1, 2, 5, 10, max_stay_len))) # label values
  stay_len_breaks <- stay_len_breaks[stay_len_breaks <= max_stay_len]

  # Set breaks for dates   (label value in legend)
  pyear_ends <- proportion_of_year(lubridate::as_date(c("2023-01-01",
                                                        "2023-12-31")))
  pyear_breaks <- seq(pyear_ends[2], pyear_ends[1], length.out = 5)

  # Set subtitle based on unique date ranges in the data
  date_ranges <- routes |>
    dplyr::group_by(.data$route) |>
    dplyr::summarize(first = dplyr::first(.data$pyear),
                     last = dplyr::last(.data$pyear)) |>
    dplyr::mutate(first = format_pyear(.data$first),
                  last = format_pyear(.data$last),
                  label = paste0(.data$first, " - ", .data$last)) |>
    dplyr::select(dplyr::last_col()) |>
    dplyr::distinct()
  subtitle <- paste(date_ranges$label, collapse = ", ")


  #----------------------------------------------------------------------------#
  # Set parameters that control plot aesthetics
  #----------------------------------------------------------------------------#

  # color palette for lines and dots
  #   Note: alpha is set in the colors rather than as an
  #         additional argument to scale_color_gradient() otherwise
  #         the legend won't have the alpha
  #         See https://github.com/tidyverse/ggplot2/issues/3103
  pal <- ggthemes::tableau_color_pal("Classic Cyclic")
  year_cols <- pal(13)
  year_cols <- c(year_cols, year_cols[1]) # to make fully cyclical
  alpha <- 0.7
  year_cols <- paste0(year_cols, as.hexmode(as.integer(alpha * 255)))

  # Set Colors for mask
  active_cell_color <- rgb(.85, .85, .85, .5)
  inactive_cell_color <- rgb(.95, .95, .95, .5)

  # Set coast appearance
  coast_linewidth <- .25
  coast_color <- grDevices::grey(.5)

  # Set route appearance
  route_linewidth <- .85

  # Set dot size (smallest, largest)
  dot_sizes <- c(1.1, 3.5)

  #----------------------------------------------------------------------------#
  # Assemble the plot
  #----------------------------------------------------------------------------#

  p <- ggplot2::ggplot(data = routes,
                       ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +

    ggplot2::guides(fill = "none") +

    ggplot2::scale_fill_manual(values = c(`TRUE` = active_cell_color,
                               `FALSE` = inactive_cell_color)) +

    # Add the raster (showing active vs inactive cells in shades of grey)
    ggplot2::geom_raster(data = rast,
                         ggplot2::aes(fill = .data$value)) +

    # Add stay dots
    # Note group is a seq along the time axis
    # and not the route (or line id) so that it works properly with
    # transition_reveal (see last example in ?transition_reveal)
    ggplot2::geom_point(
                        ggplot2::aes(size = .data$elapsed_stay,
                                     color = .data$pyear,
                                     group = seq_along(.data$pyear))) +


    # Add route lines
    ggplot2::geom_path(ggplot2::aes(color = .data$pyear,
                                    group = .data$route),
                       linewidth = route_linewidth,
                       lineend = "round") +

    # alternative color scale.  Used in prior plots. it requires adding
    #  viridis to imports.
    # viridis::scale_color_viridis(labels = format_pyear, name = "Date") +

    # Set color for route lines and stay dots
    ggplot2::scale_color_gradientn(
      colors = year_cols,
      labels = format_pyear(pyear_breaks),
      name = "Date",
      breaks = pyear_breaks,
      rescaler = scales::rescale_none,
      limits = c(0, 1),
      guide = ggplot2::guide_colourbar(reverse = TRUE))  +

    # Setup stay dot scale and legend
    ggplot2::scale_size_continuous(limits = stay_len_range,
                                   range = dot_sizes,
                                   breaks = stay_len_breaks,
                                   name = "Stay Length") +

    ggplot2::theme(strip.background = ggplot2::element_blank()) +

    # Plot coastal data
    ggplot2::geom_sf(data = coast,
                     inherit.aes = FALSE,
                     linewidth = coast_linewidth,
                     color = coast_color) +

    # coord_sf is required when using geom_sf this prevents expanding the
    #  extent of the plot beyond the data.
    ggplot2::coord_sf(expand = FALSE)

  # Add title and subtitle
  p <- p + ggplot2::ggtitle(
    label = species(bf),
    subtitle = subtitle)


  if (facet) {
    p <- p +
      ggplot2::facet_wrap(~.data$route) +

      # Remove axis labels and ticks
      # facet plot is cluttered if you leave these in.
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank()
      )
  }

  return(p)
}
