#' Animate movement vectors
#'
#' `animate_movement_vectors()` produces a `gganim` object in which each frame
#' is a map of vectors showing the average modeled movement for all birds from
#' each cell in the landscape at a given timestep. It is analogous to a series
#' of images created with [plot_movement_vectors()].
#'
#' Each arrow represents the average of all the transitions from a single cell.
#' The tail of the arrow is the center of that cell, the head is the average
#' location at the following timestep for birds that start at that cell.
#'
#' The timestep and/or date label is the starting timestep for the transition
#' that is displayed and the format depends on
#' [birdflow_options("time_format")][birdflow_options()]
#'
#' Thicker lines and less transparency (darker shading) indicate higher density
#' in the eBird S&T distribution for the beginning timestep of the displayed
#' transition.
#'
#' I'm undecided as to whether the `gganimate()` call from the example
#' should be part of the function, making it return a "gif_image" object
#' instead. It would make it easier to use but hard to adjust the rendering
#' parameters. The output object might change in a future revision.
#'
#' @param bf a BirdFlow object
#' @inheritParams lookup_timestep_sequence
#' @inheritDotParams lookup_timestep_sequence end direction season_buffer n
#'
#' @return a `gganim` object. `print()` will plot it with default
#' options, or use [animate()] to set the options. See the example for
#' recommended settings.
#'
#' @export
#'
#' @examples
#'
#' bf <- BirdFlowModels::amewoo
#' a <- animate_movement_vectors(bf)
#'
#' \dontrun{
#'
#' # Animate, display, and save
#' #   Note: "ragg_png" is considerably faster and produces cleaner output than
#' #         the default device.
#' gif <- gganimate::animate(a, fps = 1, device = "ragg_png",
#'                           width = 6, height = 5,
#'                           res = 150, units = "in")
#' print(gif)
#' path <- tempfile(fileext = ".gif") # set to desired output location
#' gganimate::anim_save(gif)
#' }
#'
animate_movement_vectors <- function(bf, start = "all", ...) {

  timesteps <- lookup_timestep_sequence(bf, start = start, ...)
  transitions <- lookup_transitions(bf, start = start, ...)

  diff <- timesteps[2] -  timesteps[1]
  if (diff %in% c(1, -1)) {
    # If first transition isn't over year boundary
    direction <- ifelse(diff == 1, "forward", "backward")
  } else {
    # if first transition is over the year boundary
    if (!timesteps[1] %in% c(1, n_timesteps(bf))) {
      stop("function logic failed expected the timestep to be",
           " 1 or ",  n_timesteps(bf), " found ", timesteps[1])
    }
    direction <- ifelse(timesteps[1] == 1, "backward", "forward")
  }

  # Create data frame with movement vectors for all timesteps
  cat("Creating vector fields\n\t")
  d <- vector(mode = "list", length = length(transitions))
  for (i in seq_len(length(timesteps) - 1)) {
    cat(".")
    d[[i]] <- calc_movement_vectors(bf, timesteps[i], direction)
  }
  cat("\n")

  d <- do.call(rbind, d)

  # This is required here so that the range rescaling is consistent
  # across all timesteps
  d$width <- range_rescale(d$weight, 0.085, .7)

  g <- plot_movement_vectors(bf, mv = d)

  subtitle <- ifelse(
    birdflow_options("time_format") == "timestep",
    "Week {current_frame}",
    "Week {current_frame}, {reformat_timestep(current_frame, bf)}")

  ga <- g +
    gganimate::transition_manual(start) +
    ggplot2::labs(title = "{species(bf)}", subtitle = subtitle)

  return(ga)
}
