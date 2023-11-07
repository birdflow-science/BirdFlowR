#' Convert a sequence of timesteps into a sequence of transition names
#'
#' @param timesteps A valid sequence of timesteps. See
#'  `lookup_timestep_sequence()`.
#' @param bf A BirdFlowR model, used to determine how much padding
#'   is needed around the timesteps.
#' @return A directional sequence of transitions that connect `timesteps`.
#' @export
as_transitions <- function(timesteps, bf) {
  padding <- get_timestep_padding(bf)
  pad <- function(x) stringr::str_pad(x, width = padding, pad = "0")
  return(paste0("T_", pad(timesteps[-length(timesteps)]), "-",
                 pad(timesteps[-1])))
}
