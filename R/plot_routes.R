
#' Plot routes
#'
#' Plot routes as lines with color indicating the passage of time and dot size
#' indicating the length of stay at each stop.
#'
#' @param points The points component of the list returned by [route()] or
#' [route_migration()]
#' @param bf A BirdFlow object
#' @param facet if TRUE then use [ggplot2::facet_wrap()] to show each route
#' out into a separate subplot.
#' @param max_stay_len Used scale the stay length dots. If omitted it will
#' be set to the maximum "stay_len" value in `points`.  Set it manually to
#' keep the dot scaling consistent across multiple plots.
#' @return a ggplot object.  Use [print()] to display it.
#' @export
#' @importFrom rlang .data
#' @examples
#'bf <- BirdFlowModels::amewoo
#'n_spring <- n_fall <- 4
#'rts <- route_migration(bf, n_spring)
#'points <- rts$points
#'plot_routes(points, bf)
#'
#'plot_routes(points, bf, facet = TRUE)
#'
#' # Create a plot with both spring and fall migrations
#' fall_rts <- route_migration(bf, n_fall, "fall")
#' fall_rts$points$route <- fall_rts$points$route + n_spring # for unique routes
#' plot_routes(rbind(points, fall_rts$points), bf)
#'
#' rts <- route_migration(bf, 5, "fall", season_buffer = 2)
#' plot_routes(rts$points, bf)
plot_routes <- function(points, bf, facet = FALSE, max_stay_len = NULL) {

  #----------------------------------------------------------------------------#
  # Data reformatting and preparation
  #----------------------------------------------------------------------------#

  # Check for full output from route() and select just point component
  if(is.list(points) && all(names(points) == c("points", "lines")) &&
                            is.data.frame(points$points)){
    points <- points$points
  }

  # Reformat and add columns to points
  # pyear is the proportion of the year using it instead of
  # date allows assigning a cyclical color scale

  points$date <- lubridate::as_date(points$date)
  points$pyear <- proportion_of_year(points$date)

  # The stops, unique locations where they stayed more than a week
  the_stops <- points[points$stay_len > 1, ]
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
    max_stay_len <- max(points$stay_len)
  # must be at least 3 so range isn't a point
  max_stay_len <- max(max_stay_len, 3)

  # Set breaks for stay length (label values in legend)
  stay_len_range <- c(2, max_stay_len) # range of stay length
  stay_len_breaks <- unique(sort(c(2, 5, 10, max_stay_len))) # label values
  stay_len_breaks <- stay_len_breaks[stay_len_breaks <= max_stay_len]

  # Set breaks for dates   (label value in legend)
  pyear_ends <- proportion_of_year(lubridate::as_date(c("2023-01-01",
                                                        "2023-12-31")))
  pyear_breaks <- seq(pyear_ends[2], pyear_ends[1], length.out = 5)

  # Determine subtitle (based on unique date ranges in the data)
  date_ranges <- points |>
    dplyr::group_by(.data$route) |>
    dplyr::summarize(first = dplyr::first(.data$pyear),
                     last = dplyr::last(.data$pyear)) |>
    dplyr::mutate(first = format_pyear(first),
                  last = format_pyear(last),
                  label = paste0(.data$first, " - ", .data$last)) |>
    dplyr::select(last_col()) |>
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

  p <- ggplot2::ggplot(data = points,
                       ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +

    ggplot2::guides(fill = "none") +

    ggplot2::scale_fill_manual(values = c(`TRUE` = active_cell_color,
                               `FALSE` = inactive_cell_color)) +

    # Add the raster (showing active vs inactive cells in shades of grey)
    ggplot2::geom_raster(data = rast,
                         ggplot2::aes(fill = .data$value)) +

    # Add stay dots
    ggplot2::geom_point(data = the_stops,
                        ggplot2::aes(size = .data$stay_len,
                                     color = .data$pyear)) +


    # Add route lines
    ggplot2::geom_path(ggplot2::aes(color = .data$pyear,
                                    group = .data$route),
                       linewidth = route_linewidth,
                       lineend = "round") +

    #   viridis::scale_color_viridis(labels = format_pyear, name = "Date") +

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
    # coord_fixed(ratio =1) +
    ggplot2::geom_sf(data = coast,
                     inherit.aes = FALSE,
                     linewidth = coast_linewidth,
                     color = coast_color) +

    ggplot2::coord_sf(expand = FALSE)

  p <- p + ggplot2::ggtitle(
    label = species(bf),
    subtitle = subtitle)


  if (facet) {
    p <- p +
      ggplot2::facet_wrap(~.data$route) +

      # Remove axis labels and ticks
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank()
      )
  }

  return(p)
}
