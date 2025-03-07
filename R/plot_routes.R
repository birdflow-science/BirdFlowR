#' @rdname plot_routes
#' @title Plot Routes
#' @description
#' Plot `Routes` and `BirdFlowRoutes` objects as as lines with color indicating
#' the passage of time. For `BirdFlowRoutes` the end point of each week
#' is shown as a dot and the size of the dot corresponds to how long the birds
#' stayed at that location.
#' @details `plot.BirdFlowRoutes()` calls `plot_routes()`.
#'
#'
#' @param routes,x An object of class `Routes` or  `BirdFlowRoutes`.  Likely the
#' the output of  [route()], [as_BirdFlowRoutes], or [Routes()].
#' @param bf A BirdFlow object. Only used if `x` is a `Routes` object, in
#' which case it provides the CRS and
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
#' @param stay_units The unit to plot the stay length at each location. Default
#' to `weeks`. Other options include `sec`, `mins`, `hours`, `days` and `weeks`.
#' @param show_mask Should the BirdFlowModel's (`bf`) static mask be displayed.
#' @param crs Only used when `bf` is missing.  `crs` sets the Coordinate
#' Reference system used for plotting. See [terra::crs()].
#' @param static For internal use. It is set to `FALSE` when `plot_routes()` is
#' called from [animate_routes()].
#' @param ... Passed to `plot_routes()` from `plot()` for `Route` and
#' `BirdFlowRoutes` objects.
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
#'new_rts <- rts
#'new_rts$data <- new_rts$data[new_rts$data$route_id %in% 1:4, ]
#'plot_routes(new_rts, bf, facet = TRUE)
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
plot_routes <- function(routes,
                        bf,
                        facet = FALSE,
                        max_stay_len = NULL,
                        use_seasonal_colors = TRUE,
                        pal = NULL,
                        barheight = 8,
                        route_linewidth = .85,
                        dot_sizes = c(1.1, 3.5),
                        coast_linewidth =  .25,
                        stay_units = "weeks",
                        show_mask = TRUE,
                        crs = NULL,
                        res = NULL,
                        static = TRUE) {

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
  # As of March 2025 the Routes and BirdFlowRoutes classes have been formalized
  # and are now built around a list object.
  #----------------------------------------------------------------------------#
  has_bf <- !missing(bf)
  has_stays <- has_bf
  if(has_bf) {
    ## Back compatibility code to guarantee the bf$dates object matches
    # the current format
    bf$dates <- get_dates(bf)
  }


  if (!has_bf && inherits(routes, "BirdFlowRoutes")) {
    # Make psuedo BirdFlow object from BirDFlowRoutes components
    bf <- new_BirdFlow()
    bf$dates <- routes$dates
    bf$geom <- routes$geom
    bf$metadata <- routes$metadata
    bf$species <- routes$species
    has_bf <- TRUE
    has_stays <- TRUE
  }

  # If "Routes" object supplied add x and y and set crs
  if(inherits(routes, "Routes") && !inherits(routes, "BirdFlowRoutes")) {
    if(has_bf) {
      has_stays <- FALSE
    }

    if(!has_bf) {
      if (is.null(crs)) {
        warning("Using latitude and longitude for plotting. This will",
                "create a lot of distortion. Use the bf or crs argument to",
                "set a coordinate reference system for plotting")
        crs <- terra::crs("EPSG:4326")
      }
      crs <- terra::crs(crs)
      has_bf <- FALSE

      # Add  x and y columns (in CRS) to data
      xy <- latlon_to_xy(lat = routes$data$lat, lon = routes$data$lon, bf = crs)
      routes$data$x <- xy$x
      routes$data$y <- xy$y
    }
  } # end Routes object

  #----------------------------------------------------------------------------#
  # Data reformatting and preparation
  #----------------------------------------------------------------------------#

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

  routes$data$date <- lubridate::as_date(routes$data$date)
  routes$data$pyear <- proportion_of_year(routes$data$date)

  # Calculate Elapsed time at location - used by animate_routes()
  if (has_stays){
    routes$data <- routes$data |>
      dplyr::group_by(.data$route_id, .data$stay_id) |>
      dplyr::mutate(elapsed_stay = as.numeric(date - min(date), units = stay_units)) |>
      as.data.frame()
  }

  # Calculate year number (input to HPY calculation)
  routes$data <- routes$data |>
    dplyr::group_by(.data$route_id) |>
    dplyr::mutate(year_number = calc_year_number(.data$date)) |>
    as.data.frame()
  if (!all(routes$data$year_number %in% c(1, 2)))
    stop("Track passes through parts of three distinct years and",
         " so cannot be plotted.")

  # CalculateHPV) 0 to 0.5 for the first year
  # 0.5 to 1 for the second year (if track crosses year boundary)
  routes$data$hpy <- 0.5 * routes$data$pyear +
    0.5 * (routes$data$year_number - 1)


  # Make raster showing which cells are active in the model
  if (has_bf && show_mask) {
    rast <- rasterize_distr(rep(TRUE, times = n_active(bf)),
                            bf, format = "dataframe")
    rast$value[is.na(rast$value)] <- FALSE
  }

  #----------------------------------------------------------------------------#
  # Data summary
  #----------------------------------------------------------------------------#
  if (has_stays) {
    # Maximum length of stay
    if (is.null(max_stay_len))
      max_stay_len <- max(routes$data$elapsed_stay)
    # must be at least 2 so range isn't a point
    max_stay_len <- max(max_stay_len, 2)

    # Set breaks for stay length (label values in legend)
    stay_len_range <- c(0, max_stay_len) # range of stay length
    stay_len_breaks <- unique(sort(c(0,
                                     round(exp(seq(log(1), log(max_stay_len), length.out = 5)))[2:5]
    ))) # label values
    stay_len_breaks <- stay_len_breaks[stay_len_breaks <= max_stay_len]

  }

  # Calculate the range and breaks in half proportional year values
  hpy_range <- range(routes$data$hpy)
  hpy_breaks <- make_pyear_breaks(hpy_range,
                                  dates = ifelse(has_bf, bf$dates, NA))

  # Set subtitle based on unique date ranges in the data
  date_ranges <- routes$data |>
    dplyr::group_by(.data$route_id) |>
    dplyr::summarize(first = dplyr::first(.data$hpy),
                     last = dplyr::last(.data$hpy)) |>
    dplyr::mutate(first = format_pyear(.data$first),
                  last = format_pyear(.data$last),
                  label = paste0(.data$first, " - ", .data$last)) |>
    dplyr::select(dplyr::last_col()) |>
    dplyr::distinct()


  if(length(date_ranges$label) == 1){
    # All same range likely either synthetic routes or
    # a single real route
    subtitle <- paste(date_ranges$label, collapse = ", ")
  } else {
    # Varied date ranges use the entire date window for routes
    # with year
    date_range <- range(routes$data$date)
    subtitle <-   paste(lubridate::month(date_range,
                                         label = TRUE,
                                         abbr = FALSE),
                        " ",
                        lubridate::day(date_range),
                        ", ",
                        lubridate::year(date_range),
                        collapse = " - ", sep = "")
  }

  plot_title <- ifelse(has_bf, species(bf), routes$species$common_name)

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

  p <- ggplot2::ggplot(data = routes$data,
                       ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank()) +

    ggplot2::guides(fill = "none") +

    ggplot2::scale_fill_manual(values = c(`TRUE` = active_cell_color,
                                          `FALSE` = inactive_cell_color))

  if (has_bf && show_mask) {
    p <- p +
      # Add the raster (showing active vs inactive cells in shades of grey)
      ggplot2::geom_raster(data = rast,
                           ggplot2::aes(fill = .data$value))
  }

  if (has_stays) {
    p <- p +
      # Add stay dots
      # Note group is a seq along the time axis
      # and not the route (or line id) so that it works properly with
      # transition_reveal (see last example in ?transition_reveal)
      ggplot2::geom_point(
        ggplot2::aes(size = .data$elapsed_stay,
                     color = .data$hpy,
                     group = seq_along(.data$hpy))) +


      # Setup stay dot scale and legend

      ggplot2::scale_size_area(limits = stay_len_range,
                               max_size = dot_sizes[2],
                               breaks = stay_len_breaks,
                               name = paste0("Stay Length\n", "(", stay_units, ")"),
                               guide = ggplot2::guide_legend(order = 0))
  } # end has stays

  p <- p +

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
                                      order = 1))

  # Plot coastal data
  if (!is.null(coast_color) && !is.null(coast_linewidth)) {

    # Coastline for this model
    if(has_bf) {
      suppress_specific_warnings({
        coast <- get_coastline(bf)
      }, "No objects within extent. Returning empty sf object.")
    } else {
      buffer_prop <- 0.05
      xmin <- min(routes$data$x)
      xmax <- max(routes$data$x)
      ymin <- min(routes$data$y)
      ymax <- max(routes$data$y)
      xbuffer <- (xmax - xmin) * buffer_prop
      ybuffer <- (ymax - ymin) * buffer_prop
      xmin  <- xmin - xbuffer
      xmax <- xmax + xbuffer
      ymin <- ymin - ybuffer
      ymax <- ymax + ybuffer

      corners <- data.frame(x = c(xmin, xmax, xmax, xmin),
                            y = c(ymin, ymin, ymax, ymax))
      sf_corners <- sf::st_as_sf(corners, coords = c("x", "y"), crs = crs)

      coast <- get_coastline(sf_corners)
    }

    if (nrow(coast) > 0) {
      p <- p +
        ggplot2::geom_sf(data = coast,
                         inherit.aes = FALSE,
                         linewidth = coast_linewidth,
                         color = coast_color)
    }
  }

  # coord_sf is required to adjust coordinates while using geom_sf
  # Here we are preventing expanding the extent of the plot.

  p  <- p +
    # Add title and subtitle
    ggplot2::ggtitle(
      label = plot_title,
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

  if(!has_bf) {
    p <- p +
      ggplot2::coord_sf(xlim = c(xmin, xmax),
                        ylim = c(ymin, ymax),
                        expand = FALSE)

  } else {
    p  <- p +
      ggplot2::coord_sf(expand = FALSE)
  }

  return(p)
}

#' @rdname plot_routes
#' @method plot BirdFlowRoutes
#' @export
plot.BirdFlowRoutes <- function(x, ...) {
  plot_routes(x, ...)
}


# Internal helper function to determine the year number associated with
# a series of dates. It returns 1 for the earliest year in dates
# and every subsequent year increments by 1.  It is agnostic to the
# order of the dates
calc_year_number <- function(dates){
  years <- lubridate::year(dates)
  year_number <- years - min(years) + 1
  return(year_number)
}

