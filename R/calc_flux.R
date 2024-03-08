if(FALSE){

  # Prep for line by line coding
  bf <- BirdFlowModels::amewoo
  if(!exists("o_between")){
    result <- is_between(bf)
    o_between <- betwween
  }
  between <- o_between
  margi

}

calc_flux <- function(bf, points = NULL, radius = NULL, n_directions = 1, format = NULL){

  if(n_directions != 1)
    stop("Only one directional flux is supported at the moment.")

  if(is.null(format)){
    if(is.null(points)){
      format <- "dataframe"
    } else {
      format <- "points"
    }
  }

  format <- tolower(format)
  stopifnot(format %in% c("points", "spatraster", "dataframe"))


  result <- is_between(bf, points, radius, n_directions)
  between <- result$between
  points <- result$points

  timesteps <- lookup_timestep_sequence(bf)
  transitions <- lookup_transitions(bf)
  marginals <- gsub("^T", "M", transitions)

  # Empty result matrix, rows are points from between columns are timesteps
  net_movement <-
    matrix(NA_real_,
           nrow = dim(between)[3],
           ncol = n_transitions(bf),
           dimnames = c(dimnames(between)[3],
                        list(transition =  transitions)))


  n_pts <- dim(between)[3]
  for(i in seq_along(marginals)){
    from <- timesteps[i]
    to <- timesteps[i + 1]
    mar <- get_marginal(bf, marginals[i])
    fdm <- get_dynamic_mask(bf, from)
    tdm <- get_dynamic_mask(bf, to)

    # subset betweenness (sb) to conform to this marginals dynamic masks
    sb <- between[fdm, tdm, ]
    stopifnot(all(dim(sb)[1:2] == dim(mar)))  # verify

    for(j in seq_len(n_pts)){
      net_movement[j, i] <- sum(mar[sb[, , j]])
    }
  }


  if (format == "points") {
    return(list(net_movement, points))
  }

  if (format == "spatraster" ) {
    raster <- array(data = NA, dim = c(nrow(bf), ncol(bf), ncol(net_movement)))
    dimnames(raster) = list(row = NULL, col = NULL,
                            transition = colnames(net_movement))
    rc <- cbind(y_to_row(points$y, bf), x_to_col(points$x, bf))
    colnames(rc) <- c("row", "col")
    for(i in seq_along(marginals)){
      # The line below uses matrix indexing of an array where each
      # row of the matrix defines a particular cell by treating the
      # values in that row as an index on the dimensions of the array
      # eg 1, 10, 40 will index rast[1, 10, 30]
      rcl <- cbind(rc, loc = i)
      raster[rcl] <- net_movement[, i]
    }
    r <- terra::rast(raster, extent = bf$geom$ext, crs = bf$geom$crs)
    names(r) <- transitions
    return(r)
  }

  if (format == "dataframe") {
    wide <- cbind(as.data.frame(points)[, c("x", "y")],
                net_movement)
    long <- tidyr::pivot_longer(wide, cols = setdiff(names(wide), c("x", "y")),
                                names_to = "transition", values_to = "movement")

    long$date <- as.character(lookup_date(long$transition, bf))

    return(long)

  }

  stop(format, "is not a recoginized format.") # shouldn't ever get here
}


if(FALSE){

  gradient_colors <-
    c("#EDDEA5", "#FCCE25", "#FBA238", "#EE7B51", "#DA596A", "#BF3984",
      "#9D189D", "#7401A8", "#48039F", "#0D0887")
  title <- paste0(species(bf), " Net Movement")

    p <- long |>
      #dplyr::filter(transition %in% transitions[seq(4, 50, 4)]) |>
      ggplot(aes(x = x, y = y, fill = .data$movement)) +
        geom_raster() +
        ggplot2::scale_fill_gradientn(colors = gradient_colors) +
        facet_wrap(vars(.data$transition))

    anim <- p +
      facet_null() +
      gganimate::transition_manual(frames = .data$date) +
      ggplot2::labs(title = title,
                    subtitle = "{current_frame}")


    gif <- gganimate::animate(anim, fps = 3, device = "ragg_png",
                              width = 7, height = 6, res = 100, units = "in")

    gganimate::save_animation(gif, file = "C:/temp/amewoo_net_movement.gif")

  facet_wrap(facets = .data$transition)

}

