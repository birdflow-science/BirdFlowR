#' Animate routes
#'
#' Animate synthetic routes produced by [route()] and [route_migration()],
#' producing a dynamic version of [plot_routes()].
#'
#' Note when rendering early frames (at a minimum the first) there will only
#' one point per route, resulting in a message:
#' "geom_path()`: Each group consists of only one observation.
#'  ℹ Do you need to adjust the group aesthetic?"
#'  This will possibly be repeated while individuals remain in one location.
#'  It can be safely ignored. The error is thrown while rendering and not
#'  from within `animate_routes()` where I could suppress it.
#'
#' @inheritParams plot_routes
#'
#' @inherit animate_movement_vectors return
#' @export
#'
#' @examples
#'
#'
#' bf <- BirdFlowModels::amewoo
#' rts <- route_migration(bf, 5)
#' anim <- animate_routes(rts, bf)
#'
#' \dontrun{
#'   # example render
#'   timesteps <- unique(rts$points$timestep)
#'   gif <- gganimate::animate(anim,
#'                             device = "ragg_png", # ragg_png is fast and pretty
#'                             width = 6, height = 5,
#'                             res = 150, units = "in",
#'                             nframes = length(timesteps) * 4, fps = 4)
#'
#'   # Display
#'   print(gif)
#'
#'   # Save
#'   gif_file <- tempfile("animation", fileext = ".gif")
#'   gganimate::save_animation(gif, gif_file)
#'   file.remove(gif_file) # cleanup
#
#' }
#'
animate_routes <- function(routes, bf, max_stay_len ){
  p <- plot_routes(routes, bf)


  ring_size <- 4 # point size of the ring symbol used for current location


  # Add a second point layer to plot a ring at the current location
  a <- p +
    ggplot2::geom_point(
               size = ring_size,
               shape = 1,
               mapping = ggplot2::aes(group = .data$route,
                             color = .data$pyear)) +

    # Animate and add dynamic subtitle
    gganimate::transition_reveal(.data$pyear) +
    ggplot2::labs(title = "{species(bf)}", subtitle = "{format_pyear(frame_along)}" )

  return(a)
}
