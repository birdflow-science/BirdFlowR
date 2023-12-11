#' Convert a sequence of timesteps into a sequence of transition names
#'
#' @param timesteps A valid sequence of timesteps. See
#'  `lookup_timestep_sequence()`.
#' @param bf A BirdFlowR model, used to determine how much padding
#'   is needed around the timesteps.
#' @return A directional sequence of transitions that connect `timesteps`.
#' @export
as_transitions <- function(timesteps, bf) {
  return(paste0("T_", pad_timestep(timesteps[-length(timesteps)], bf),
                "-",
                pad_timestep(timesteps[-1], bf)))
}
