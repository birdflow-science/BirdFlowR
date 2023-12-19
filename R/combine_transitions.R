#' Function to combine a sequence of transition matrices into one
#'
#' @param bf A BirdFlow object
#' @inheritDotParams lookup_transitions -x
#'
#' @return This returns the transition probabilities associated with a
#' sequence of timesteps. It will have a column for every unmasked cell
#' at the starting timestep and a row for every unmasked cell in the
#' last timestep, with cell values being the probably of transitioning from
#' that row to that column between the start and end of the time sequence
#' described by `...`
#'
#' @export
combine_transitions <- function(bf, ...){
  # Lookup transition names
  transitions <- lookup_transitions(x = bf, ...)

  # Multiply all the transitions together
  for(i in seq_along(transitions)){
    # Origin timestep get transtion
    if(i == 1){
      trans <- get_transition(bf,  transitions[i])
      next
    }
    # All other timesteps get transition and multiple with prior
    a <- get_transition(bf, transitions[i])
    trans <- a %*% trans
  }
  return(trans)
}
