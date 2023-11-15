

#' Internal function to make a marginal index for a BirdFlow object.
#'
#' `make_marginal_index()` is called by `[import_birdflow()]`
#' and `truncate_birdflow()` to add a marginal index to a BirdFlow object.
#'
#' [n_transitions()], [n_timesteps()], [is_cyclical()], and
#' [get_timestep_padding()] need to work on the object, which means that
#' `metadata$n_timesteps`, `metatdata$n_transitions`, and
#' `metadata$timestep_padding` should all  be properly set.
#' The marginals don't have to exist yet.
#'
#' @param bf A BirdFlow object
#'
#' @return A data.frame that facilitates looking up marginals from transition
#' names.  It has columns:
#' \item{from}{starting timestep for transition (direction matters)}
#' \item{to}{ending timestep for transition}
#' \item{direction}{transition direction, either "forward", or "backward"}
#' \item{transition}{transition name e.g. "T_02-02"}
#' \item{marginal}{marginal name e.g. "M_01-02", order matches
#'      forward transition order, so smaller number is generally first
#'     except with the last marginal in a circular model e.g. "M_52-01"}
#'
#' @keywords internal
make_marginal_index <- function(bf) {
  # n_timesteps, n_transitions, and metadata$timestep_padding should be
  # properly set in bf

  # Marginals do not need to exist.

  # Save marginal index - allows looking up a marginal, and direction from
  # a transition code
  # Columns:
  #   from : timstep
  #    to : timestemp
  #    direction : forward or backward
  #    transition : transition code e.g. ("T_01-02", is directional)
  #    marginal : marginal code e.g. "M_01-02", order follows
  #      forward transition order, so smaller number is generally first
  #      except with the last marginal in a circular model e.g. "M_52-01"
  if (is_cyclical(bf)) {
    index <- data.frame(from = 1:n_timesteps(bf), to = c(2:n_timesteps(bf), 1),
                        direction = "forward")
  } else {
    index <- data.frame(from = 1:(n_timesteps(bf) - 1), to = 2:n_timesteps(bf),
                        direction = "forward")
  }
  index <- rbind(index, data.frame(from = index$to, to = index$from,
                                   direction = "backward"))


  index$transition <- paste0("T_", pad_timestep(index$from, bf), "-",
                             pad_timestep(index$to, bf))
  index$marginal <- NA
  sv <- index$direction == "forward" # selection vector
  index$marginal[sv] <-
    paste0("M_", pad_timestep(index$from[sv], bf), "-",
           pad_timestep(index$to[sv], bf))
  sv <- index$direction == "backward"
  index$marginal[sv] <-
    paste0("M_", pad_timestep(index$to[sv], bf), "-",
           pad_timestep(index$from[sv], bf))

  return(index)
}
