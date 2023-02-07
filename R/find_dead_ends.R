#' function to identify inconsistencies in sparse BirdFlow models
#'
#' With sparsification ([sparsify()]) it's possible to create models that
#' have dead ends -  states that can be entered but not exited. This occurs when
#'  one marginal encodes transitions into a state for which the next marginal
#'  has no transitions out.
#'
#'  Consider two adjacent marginals; the rows of the second and the columns of
#'  the first both correspond with the species distribution for the timestep
#'  between them. For every location in the model at that timestep
#'  there are four possibilities (1) the first marginal's column doesn't sum
#'   to zero and the second marginal's row does then there is a forward
#'   transition into that state but no forward transition out and it's a
#'   forward dead end; (2) the situation is reversed and first marginals column
#'   sums to zero and the second marginal's corresponding row does not than
#'  there is a backward dead end; (3) if they both sum to zero the model is fine
#'   but that state is dropped; and (4) if they both have non-zero sums than the
#'  corresponding state is valid and can be reached and exited when projecting
#'  forward or backwards.
#'
#'  Dead ends result in lost density with [forecast()] and errors when
#'  they are entered with [route()]. Based on initial testing the transitions
#'  into the dead end are often low probability so routing may work most of the
#'  time but occasionally fail. The error will occur with the subsequent
#'  iteration when attempting to sample from a bunch of zero
#'   probability states.
#'
#' @param x `BirdFlow` model
#'
#' @return a data.frame with columns:
#'| `timestep` | the timestep associated with the dead end |
#'| --- | --- |
#'| `direction` | either `"forward"` or `"backward"` indicating which direction the dead end is encountered in |
#'| `i` | the index of the model state that has a dead end |
#'| `mar` | the marginal which leads into the dead end (this marginal has non-zero value in the i'th column if direction is forward and i'th row if direction is backward) |
#'| `x`, and `y` | the x and y coordinates corresponding with state `i` |
#'
#' There will be a row for each dead end state, if no dead ends are found an
#' empty (zero row) data.frame is returned.
#'
#' @export
#'
#' @examples
#' library(BirdFlowModels)
#' bf <- amewoo
#' head(find_dead_ends(amewoo))
#'
find_dead_ends <- function(x){
  if(! has_marginals(x)){
    stop("x lacks marginals can't find dead ends.")
  }

  index <- x$marginals$index
  f_index <- index[index$direction == "forward", ]
  problems <- vector(mode = "list", length = n_timesteps(x))
  for(timestep in seq_len(n_timesteps(x))){
    # Two consecutive marginal names:
    first <-f_index$marginal[f_index$to == timestep]
    second <- f_index$marginal[f_index$from == timestep]

    if(length(first) == 0 || length(second) == 0){
      warning("Timestep ", timestep, " doesn't have two marginals")
      next
    }

    # m1 is the marginal before this timestep
    # m2 is the marginal after this timestep
    m1 <- x$marginals[[first]]
    m2 <- x$marginals[[second]]

    # Zero state indices for this timestep based on each
    # marginal
    zi1  <- Matrix::colSums(m1) == 0
    zi2 <- Matrix::rowSums(m2) == 0

    if(!all(zi1 == zi2)){   # any problems (inconsistencies in states that are zero)
      # Define forward and backward problem indices
      # they indicate states which represent dead ends in the given direction

      fpi <- which(!zi1 & zi2)    # forward problem index
      bpi <- which(zi1  & !zi2)  # backward problem index

      # assemble data frame of problems found for this timestep
      p <- NULL
      if(length(fpi) > 0){
        p <- rbind(p, data.frame(timestep = timestep,
                                 direction = "forward",
                                 i = fpi,
                                 mar = first))
      }
      if(length(bpi) > 0){
        p <- rbind(p, data.frame(timestep = timestep,
                                 direction = "backward",
                                 i = bpi, mar = second) )

      }
      problems[[timestep]] <- p
    }
  } # end for each timestep

  probs <- do.call(rbind, problems)
  if(is.null(probs)){
    return(data.frame(timestep = integer(0),
                      direction = character(0),
                      i = integer(0),
                      mar = character(0),
                      x = numeric(0),
                      y = numeric(0)
                      ))
  }
  probs <- cbind(probs, i_to_xy(probs$i, x))
  return(probs)


}
