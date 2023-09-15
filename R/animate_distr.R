#' Animate distributions
#'
#' Animate distributions as produced by `get_distr()`,
#' `as_distr()`, or `predict()`.  The distributions will be displayed
#' in the column order in `distr` and column labels will be used as
#' plot subtitles.
#'
#' @param distr A set of distributions it should be a matrix with `n_active(bf)` rows
#' and a column for each distribution. The animation will proceed in the order
#  in column order and column names will be used a subtitles in the plot.
#' @param bf The BirdFlow object that `distr` is associated with.
#' @param title The title to use for the animation. The defualt is
#' the common name of the species.
#' @inheritDotParams plot_distr -timestep
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
#' anim <- animate_distr(distr, bf)
#'
#' ### Project a distribution
#'
#' # Make starting distribution
#' # Since we define the point in WGS84 (not crs(bf)) we also have to provide
#' the crs.
#' point <- data.frame(x = -90, y = 35)
#' d1 <- as_distr(point, bf, crs = "EPSG:4326" )
#'
#' # Project
#' flow <- predict(bf, d1, season = "prebreeding")
#' pred_anim <- animate_distr(flow, bf, relative_density = TRUE)
#'
#' # This is my problem
#' #  https://stackoverflow.com/questions/76812935/change-colour-scales-in-gganimate
#' \dontrun{
#'   # example render
#'   gif <- gganimate::animate(anim,
#'                             device = "ragg_png", # ragg_png is fast and pretty
#'                             width = 7, height = 6,
#'                             res = 150, units = "in")
#'   # Display
#'   print(gif)
#'
#'   # Save
#'   gif_file <- tempfile("animation", fileext = ".gif")
#'   gganimate::save_animation(gif, gif_file)
#'   file.remove(gif_file) # cleanup
#'   }
animate_distr <- function(distr, bf, title = species(bf), ...){

   p <- plot_distr(distr, bf, ...)

  # Setup cross walk between animation frame (step) and date label
  ut <- unique(p$data$time) # unique in original order of appearance
  frames <- data.frame(frame = seq_along(ut), label = ut )

  # Add frame to data
  p$data$frame <- frames$frame[match(p$data$time, frames$label)] # 1:n  repeating

  # Drop faceting and add animation
  a <- p +
    facet_null() +
    gganimate::transition_manual(frames = frame) +
    ggplot2::labs(title = title,
                  subtitle = "{frames$label[frames$frame == current_frame]}")

  return(a)

}
