#' Animate flux
#'
#' Animate net migration from [calc_flux()].
#'
#' @inheritParams plot_flux
#' @inheritDotParams plot_flux
#'
#' @return A [`gganim`][gganimate::gganimate-package] object
#' @export
#' @inherit calc_flux examples
#' @seealso [calc_flux()],[plot_flux()]
animate_flux <- function(flux, bf, title = species(bf), ...) {
  p <- plot_flux(flux, bf, ...)

  anim <- p +
    ggplot2::facet_null() +
    gganimate::transition_manual(frames = .data$date) +
    ggplot2::labs(title = title,
                  subtitle = "{current_frame}")

  return(anim)
}
