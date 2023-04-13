#'
#' Lookup a series of transitions connecting two dates or timesteps
#'
#' `lookup_transitions()` returns an ordered vector of transition names that
#' connect start to end. If `start` and `end` are dates than their order
#' determines whether the transitions flow forward or backward in time.  If they
#' are timesteps than the `direction` argument should be used to indicate
#' whether to project "forward" or "backward" in time possibly passing the year
#' boundary.
#'
#' @details Transitions are named "T_\[from\]-\[to\]" where \[from\] and \[to\]
#'   are timesteps padded with zeros. Direction is important; "T_03-04"
#'   represents a transition backward in time.
#' @inheritParams lookup_timestep_sequence
#'
#' @return A character vector with the named transitions required to get between
#'   `start` and  `end`
#' @keywords internal
lookup_transitions <- function(x, start, end, direction, season_buffer){

  stopifnot(inherits(x, "BirdFlow"))
  steps <- lookup_timestep_sequence(x = x, start, end, direction, season_buffer)

  nc <- nchar(n_timesteps(x))
  pad <- function(x) stringr::str_pad(x, width = nc, pad = "0")

  return( paste0("T_", pad(steps[-length(steps)]), "-", pad(steps[-1]) ) )

}
