
calculate_movement_vectors <- function(bf, start, end){



  d$width <- range_rescale(d$weight, .15, .7)
  coast <- get_coastline(bf)


  start <- lookup_timestep(start, bf)
  if(missing(end))
    end <- ifelse(start == n_timesteps(bf), 1, start + 1)

  end <- lookup_timestep(end, bf)
  time_diff <- end - start
  if(end == 1  && start == n_timesteps(bf))
    time_diff = 1
  if(start == 1 && end == n_timesteps(bf))
    time_diff = -1
  stopifnot(time_diff %in% c(1, -1))
  direction <- ifelse(time_diff == 1, "forward", "backward")
  trans_name <- lookup_transitions(bf, start, end, direction)
  stopifnot(length(trans_name) == 1)

  trans <- get_transition(bf, trans_name)
  start_dm <- get_dynamic_mask(bf, start)
  start_loc <- which(start_dm)
  start_x  <- i_to_x(start_loc, bf)  # x location associated with each starting pos
  start_y  <- i_to_y(start_loc, bf)
  end_dm <- get_dynamic_mask(bf, end)
  end_loc <- which(end_dm)

  # x and y locations associated with each ending position
  end_x <- i_to_x(end_loc, bf)
  end_y <- i_to_y(end_loc, bf)

  start_distr <- get_distr(bf, start)
  end_distr <- get_distr(bf, end)

  distr_weights <- start_distr[start_dm]

  mean_end_x <- apply( trans * end_x, 2, sum)
  mean_end_y <- apply( trans * end_y, 2, sum)


  d <- data.frame(start_x, start_y, end_x = mean_end_x, end_y = mean_end_y,
                  weight = distr_weights)

  no_transition <- apply(trans, 2, sum) == 0

  d <- d[!no_transition, , drop = FALSE]

  return(d)
}

