#' Animate BirdFlow Migration Traffic Rate (BMTR)
#'
#' Animate migration traffic rates produced by [calc_bmtr()].
#'
#' @inheritParams plot_bmtr
#' @inheritDotParams plot_bmtr
#'
#' @return A [`gganim`][gganimate::gganimate-package] object
#' @export
#' @inherit calc_bmtr examples
#' @seealso [calc_bmtr()],[plot_bmtr()]
animate_bmtr <- function(bmtr, bf, title = species(bf), ...) {
  p <- plot_bmtr(bmtr, bf, ...)

  anim <- p +
    ggplot2::facet_null() +
    gganimate::transition_manual(frames = .data$label) +
    ggplot2::labs(title = title,
                  subtitle = "{current_frame}")

  return(anim)
}




#' Animate Bird Flow Migration Traffic Rate (BMTR)
#'
#' DEPRECATED FUNCTION.  Please use [animate_bmtr()] instead.
#' @inheritDotParams animate_bmtr -bmtr
#' @param flux the output from [calc_bmtr()] or, deprecated, [calc_flux()]
#' @inherit animate_bmtr return
#' @export
animate_flux <- function(flux, ...){
  warning("animate_flux() is deprecated. ",
          "Please use animate_bmtr() instead.")
  animate_bmtr(flux, ...)
}

