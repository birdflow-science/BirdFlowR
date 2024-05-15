#' Animate distributions
#'
#' Animate distributions as produced by `get_distr()`,
#' `as_distr()`, or `predict()`.  The distributions will be displayed
#' in the column order in `distr` and column labels will be used as
#' plot subtitle.
#'
#' @param distr A set of distributions it should be a matrix with `n_active(bf)`
#' rows and a column for each distribution. The animation will proceed in the
#' in column order and column names will be used a subtitles in the plot.
#' @param bf The BirdFlow object that `distr` is associated with.
#' @param title The title to use for the animation. The default is
#' the common name of the species.
#' @inheritDotParams plot_distr -distr -bf -title
#'
#' @return A gganimate object that can be displayed with `print()` or
#'  or `gganimate::animate()`.  See example for how to export to a file.
#' @export
#'
#' @examples
#'
#' # Animate distributions from BirdFlow object - derived from
#' # eBird Status and Trends:
#'
#' bf <- BirdFlowModels::amewoo
#' ts <- lookup_timestep_sequence(bf, season = "prebreeding")
#' distr <- get_distr(bf, ts)
#' anim <- animate_distr(distr, bf,  show_dynamic_mask = TRUE)
#'
#' \dontrun{
#'   # Display it
#'   anim
#' }
#' ### Project a distribution
#'
#' # Make starting distribution
#' # Since we define the point in WGS84 (not crs(bf)) we also have to provide
#' # the crs.
#' point <- data.frame(x = -90, y = 35)
#' d1 <- as_distr(point, bf, crs = "EPSG:4326" )
#'
#' # Project - density will spread over type resulting in a vastly different
#' # range of values
#' density_spread <- predict(bf, d1, season = "prebreeding")
#'
#' # Have the color gradient rescaled to the range of data in each
#' # individual frame  - density scaling is dynamic.
#' spread_anim <- animate_distr(density_spread, bf,   dynamic_scale= TRUE)
#'
#' # Or put in values to use for the limits of the color scale - values outside
#' # of the limits will be truncated
#' spread_anim <- animate_distr(density_spread, bf,  limit = c(0, 0.05))
#'
#'
#' \dontrun{
#'   # example render fo file
#'   gif <- gganimate::animate(spread_anim,
#'                             device = "ragg_png", # fast and pretty
#'                             width = 7, height = 6,
#'                             res = 150, units = "in")
#'   # Display
#'   print(gif)
#'
#'   # Save
#'   gif_file <- tempfile("animation", fileext = ".gif")
#'   gganimate::save_animation(gif, gif_file)
#'   file.remove(gif_file) # cleanup
#' }
animate_distr <- function(distr, bf, title = species(bf), ...) {

  p <- plot_distr(distr, bf, ...)

  # Drop faceting and add animation
  a <- p +
    ggplot2::facet_null() +
    gganimate::transition_manual(frames = .data$label) +
    ggplot2::labs(title = title,
                  subtitle = "{current_frame}")

  return(a)
}
