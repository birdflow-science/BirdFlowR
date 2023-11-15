#' @rdname plot_routes
#' @title Plot Routes
#' @description
#' Plot routes as lines with color indicating the passage of time and dot size
#' indicating the length of stay at each stop.
#' @details `plot.BirdFlowRoutes()` calls `plot_routes()`.
#'
#' As of 6/13/2023 `route()` returns an object of class `BirdFlowRoutes`
#' that is a data frame with some extra attributes tacked on.
#'
#' That `route()` returns a data frame like object that contains the data
#' formerly in the `points` component with columns as described here is, I think
#' finalized.
#'
#' However, whether we keep it an S3 class and whether we keep the extra
#' attributes is experimental. If you want to be defensive and not use the
#' experimental aspects then call `as.data.frame(rts)` to convert to a standard
#' data.frame.
#'
#' @param routes The output of [route()] or a similarly structured data frame.
#' @param bf,x A BirdFlow object.
#' @param facet If `TRUE` then use [ggplot2::facet_wrap()] to show each route
#' out into a separate subplot.
#' @param max_stay_len Used to scale the stay length dots. If `NULL`
#' (the default) it will be set to the maximum `"stay_len"` value in `routes`.
#' Set it manually to keep the dot scaling consistent across multiple plots.
#' @param use_seasonal_colors If `TRUE` a color scale that uses blues, greens,
#' yellows, reds, for winter, spring, summer, and fall will be used with a
#' consistent mapping of dates to colors regardless of the range of dates
#' plotted. If `FALSE` then the data will be plotted using the full color scale.
#' @param pal The color palette to use for plotting when `use_seasonal_cols` is
#'`FALSE`. Defaults to [viridisLite::viridis(n = 5)][viridisLite::viridis()].
#' @param barheight The height of the color gradient legend bar. Passed to
#' [ggplot2::guide_colorbar()] as `barheight` argument. Depending on the output
#' resolution and plot size this may need to be adjusted. Can take a number or
#' the output from [ggplot2::unit()].
#' @param route_linewidth Line width used for routes.
#' @param coast_linewidth Line width used for coastlines.
#' @param dot_sizes Two numbers indicating the smallest and largest dot sizes
#'  used to represent stay length.
#' @param ... Passed to `plot_routes()`
#'
#' @return A ggplot object. Use [print()] to display it.
#' @export
#' @importFrom rlang .data
#' @examples
#'bf <- BirdFlowModels::amewoo
#'n <- 10
#'rts <- route(bf, n, season = "prebreeding")
#'
#'# Multiple routes on one plot
#'plot_routes(rts, bf)
#'
#'# One panel per route
#'plot_routes(rts[rts$route_id %in% 1:4, ], bf, facet = TRUE)
#'
#'# Returned plot object can be edited
#'# Here we change the title and add an additional sf
#'# layer with country boundaries
#'library(ggplot2)
#'p <- plot_routes(rts, bf) +
#'  ggtitle(paste0(species(bf), " (with countries)")) +
#'  geom_sf(data = get_countries(bf),  inherit.aes = FALSE,  fill = NA) +
#'  coord_sf(expand = FALSE)
#' p
#'\dontrun{
#'# Use alternate color palettes
#' plot_routes(rts, bf,  use_seasonal_colors = FALSE )
#'
#' plot_routes(rts, bf, use_seasonal_colors = FALSE,
#'             pal = c("red", "yellow", "blue"))
#'}
plot_routes <- function(routes, bf, facet = FALSE, max_stay_len = NULL,
                        use_seasonal_colors = TRUE, pal = NULL,
                        barheight = 8,
                        route_linewidth = .85,
                        dot_sizes = c(1.1, 3.5),
                        coast_linewidth =  .25) {

  # ggplot2 translates values to a color gradient based on a range of 0 to 1
  #    This usually means that the color variable is rescaled to that range
  #    and the full color gradient is then used.
  #
  # How time is treated within this function is complicated to meet two
  # color desires:
  #   1. Allow plotting over year boundary
  #   2. Allow setting a cyclical color palette and by default mapping dates
  #      to that palette consistently regardless of the date range in any
  #      particular set of routes.
  #
  #  To achieve both of these, dates within each route are represented as
  #  half proportional years (HPY).  For any date
  #  a track prior to crossing the year boundary HPY is 0.5 x the proportion
  #  of the year that has elapsed by
  #  the date.  If, within an individual route, the year boundary is crossed
  #  than 0.5 is added to the HPV calculation for all points in the track
  #  after the year boundary.
  #  Thus:
  #  * A two year period is represented by a HPV range of 0 to 1.
  #  * All tracks up to one year in length will start in the range
  #    0 to 0.5 and end at or below 1
  # * Within any given track up to a  year in length HPV will be monotonically
  #   increasing.
  # * HPV values are independent of the particular year the track starts in.
  # For this to work consistently the seasonal color palette is duplicated
  # such that it completes two cycles in the 0 to 1 range.


  #----------------------------------------------------------------------------#
  # Make psuedo BirdFlow object
  #
  # This is a little bit of hack.  As of 6/13/2023 I've added some components
  # of the BirdFlow object to the attributes of the routes produced by
  # `route()` this extracts those and puts them back into a new BirdFlow
  # object that can used as a reference where needed by this function.  Note,
  # however, that it doesn't contain many key components of a normal BirdFlow
  # object.
  #
  # If we like having the BirdFlowRoutes class - and putting all this extra
  # stuff in it, we might want to make methods for a lot of the called functions
  # that work directly with the routes object instead of making a fake BirdFlow
  # object to use locally.
  #
  #----------------------------------------------------------------------------#
  if (missing(bf) && inherits(routes, "BirdFlowRoutes")) {
    bf <- new_BirdFlow()
    bf$dates <- attr(routes, "dates")
    bf$geom <- attr(routes, "geom")
    bf$metadata <- attr(routes, "metadata")
    bf$species <- attr(routes, "species")
  }

  #----------------------------------------------------------------------------#
  # Data reformatting and preparation
  #----------------------------------------------------------------------------#

  # Check for full output from route() and select just point component
  # This is to ease the transition in return format for route()
  # (from a list with $points and $lines to just the $points component)
  if (is.list(routes) && all(names(routes) == c("points", "lines")) &&
      is.data.frame(routes$points)) {
    routes <- routes$points
  }

  # Reformat and add additional columns to routes
  # date is a date formatted version of the original character date
  # pyear is the date represented as proportion of year (0 to 1 for full year)
  # hpy is half the proportion of the year.
  #   hpy in 0 to 0.5 represent year 1
  #   hpy 0.5 to 1 represent year 2
  # year_number Starts with 1 and increments everytime the year boundary is
  #   passed.  Should be 1 or 2 for all track points.
  # elapsed_stay is the time spent (so far) at a given stay eg if the
  #  stay_len is 4 there will be values 1:4 for the four points that
  #  represent that stay.

  routes$date <- lubridate::as_date(routes$date)
  routes$pyear <- proportion_of_year(routes$date)

  # Calculate Elapsed time at location - used by animate_routes()
  routes <- routes |>
    dplyr::group_by(.data$route_id, .data$stay_id) |>
    dplyr::mutate(elapsed_stay = dplyr::row_number()) |>
    as.data.frame()

  # Calculate year number (input to HPY calculation)
  routes <- routes |>
    dplyr::group_by(.data$route_id) |>
    dplyr::mutate(year_number = calc_year_number(.data$timestep)) |>
    as.data.frame()
  if (!all(routes$year_number %in% c(1, 2)))
    stop("Track passes through parts of three distinct years and",
         " so cannot be plotted.")

  # CalculateHPV) 0 to 0.5 for the first year
  # 0.5 to 1 for the second year (if track crosses year boundary)
  routes$hpy <- 0.5 * routes$pyear + 0.5 * (routes$year_number - 1)

  # Make raster showing which cells are active in the model
  rast <- rasterize_distr(rep(TRUE, n_active(bf)), bf, format = "dataframe")
  rast$value[is.na(rast$value)] <- FALSE
  rast$value <- rast$value

  # Coastline for this model
  coast <- get_coastline(bf)

  #----------------------------------------------------------------------------#
  # Data summary
  #----------------------------------------------------------------------------#

  # Maximum length of stay
  if (is.null(max_stay_len))
    max_stay_len <- max(routes$elapsed_stay)
  # must be at least 2 so range isn't a point
  max_stay_len <- max(max_stay_len, 2)

  # Set breaks for stay length (label values in legend)
  stay_len_range <- c(1, max_stay_len) # range of stay length
  stay_len_breaks <- unique(sort(c(1, 2, 5, 10, max_stay_len))) # label values
  stay_len_breaks <- stay_len_breaks[stay_len_breaks <= max_stay_len]

  # Calculate the range and breaks in half proportional year values
  hpy_range <- range(routes$hpy)
  hpy_breaks <- make_pyear_breaks(hpy_range, bf)

  # Set subtitle based on unique date ranges in the data
  date_ranges <- routes |>
    dplyr::group_by(.data$route_id) |>
    dplyr::summarize(first = dplyr::first(.data$hpy),
                     last = dplyr::last(.data$hpy)) |>
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
  if (use_seasonal_colors) {
    pal <- ggthemes::tableau_color_pal("Classic Cyclic")
    year_cols <- pal(13)
    year_cols <- c(year_cols, year_cols, year_cols[1]) # two cycles
    alpha <- 0.7
    year_cols <- paste0(year_cols, as.hexmode(as.integer(alpha * 255)))
    pal <- year_cols
    rescaler <- scales::rescale_none

  } else {
    if (is.null(pal)) {
      pal <- viridisLite::viridis(n = 5)
    }
    rescaler <- scales::rescale
  }

  # Set Colors for mask
  active_cell_color <- rgb(.85, .85, .85, .5)
  inactive_cell_color <- rgb(.95, .95, .95, .5)

  # Set coast appearance
  coast_color <- grDevices::grey(.5)

  #----------------------------------------------------------------------------#
  # Assemble the plot
  #----------------------------------------------------------------------------#

  p <- ggplot2::ggplot(data = routes,
                       ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank()) +

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
                   color = .data$hpy,
                   group = seq_along(.data$hpy))) +

    # Add route lines
    ggplot2::geom_path(ggplot2::aes(color = .data$hpy,
                                    group = .data$route_id),
                       linewidth = route_linewidth,
                       lineend = "round") +

    # Set color for route lines and stay dots
    ggplot2::scale_color_gradientn(
      colors = pal,
      labels = format_pyear(hpy_breaks),
      name = "Date",
      breaks = hpy_breaks,
      rescaler = rescaler,
      limits = hpy_range,
      guide = ggplot2::guide_colorbar(reverse = TRUE,
                                      barheight = barheight,
                                      order = 1)) +

    # Setup stay dot scale and legend
    ggplot2::scale_size_area(limits = stay_len_range,
                             max_size = dot_sizes[2],
                             breaks = stay_len_breaks,
                             name = "Stay Length",
                             guide = ggplot2::guide_legend(order = 0)) +

    # Plot coastal data
    ggplot2::geom_sf(data = coast,
                     inherit.aes = FALSE,
                     linewidth = coast_linewidth,
                     color = coast_color) +

    # coord_sf is required to adjust coordinates while using geom_sf
    # Here we are preventing expanding the extent of the plot.
    ggplot2::coord_sf(expand = FALSE) +

    # Add title and subtitle
    ggplot2::ggtitle(
      label = species(bf),
      subtitle = subtitle)

  if (facet) {
    p <- p +
      ggplot2::facet_wrap(~.data$route_id) +

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

#' @rdname plot_routes
#' @method plot BirdFlowRoutes
#' @export
plot.BirdFlowRoutes <- function(x, ...) {
  plot_routes(x, ...)
}
