

#' Plot bird movement vectors
#'
#' This plots the mean movement out of every cell in a BirdFlow model for a
#' given starting time, based on the transition probabilities for the following
#' transition.  Each arrow starts in a cell center and ends at the average
#' location projected for Birds from that cell. The line width and alpha
#' (transparency) are set such that arrows have more visual weight if there
#' is a higher probability of a bird being at the starting location and time.
#'
#' Importantly, this is a visual representation of the transitions encoded in
#' the model, not of the average movement of a bird through a given cell at a
#' given point in time, which would involve both the birds starting at that
#' cell and birds passing through it from other starting locations.
#'
#' @param bf a BirdFlow object
#' @inheritParams calc_movement_vectors
#' @param mv This is optional and primarily for internal use by
#' [animate_movement_vectors()] it allows explicitly providing movement vectors
#' as a data frame, in which case `start` and `direction` are ignored.
#'
#' @return an object that inherits classes `gg` and `ggplot` it can be plotted
#' with `print()`.
#' @seealso
#' - [calc_movement_vectors()] for the vector calculations.
#' - [animate_movement_vectors()] to produce animations of the vectors over
#'   time.
#' @export
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' plot_movement_vectors(bf, start = 7)
#'
#'
plot_movement_vectors <- function(bf,  start = 1, direction = "forward", mv) {

  if (missing(mv)) {
    mv <- calc_movement_vectors(bf, start, direction)
  } else {
    # mv is passed when assembling an animation and will have varying
    #  start values.
    if (all(mv$start == mv$start[1])) {
      start <- mv$start[1]
    } else {
      start <- NA
    }
  }
    coast <- get_coastline(bf)


    g <- ggplot2::ggplot() +

      # Coastline (want to be static)
      ggplot2::geom_sf(data = coast,
                       inherit.aes = FALSE,
                       mapping =  ggplot2::aes(linewidth = 0.1)) +
      ggplot2::coord_sf(expand = FALSE)  +


      ggplot2::geom_segment(
        inherit.aes = FALSE,
        data = mv,
        mapping = ggplot2::aes(x = .data$start_x,
                               y = .data$start_y,
                               xend = .data$end_x,
                               yend = .data$end_y,
                               col = .data$weight,
                               linewidth = .data$width,
                               group = .data$i),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"),
                               ends = "last",
                               type = "open")) +


      ggplot2::scale_linewidth_identity() +
      ggplot2::scale_color_gradient(low = rgb(0, 0, 0, .25),
                                     high = rgb(0, 0, 0, 1)) +
      ggplot2::guides(col = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_blank())

    if (!is.na(start)) {
      subtitle <- ifelse(
        birdflow_options("time_format") == "timestep",
        paste0("Week ", start),
        paste0("Week ", start, ", ", reformat_timestep(start, bf)))

      g <- g + ggplot2::labs(title = species(bf), subtitle = subtitle)
    }

    return(g)
  }
