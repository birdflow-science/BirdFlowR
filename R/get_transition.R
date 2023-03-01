#' Return a transition matrix from a BirdFlowR model
#'
#' This function retrieves a transition matrix from a BirdFlow object,
#' possibly calculating it from a marginal.
#'
#' @param x a BirdFlow object
#' @param transition a transition code, e.g. "T_01-02"
#'
#' @return a transition matrix
#'
#' @details `get_transition()` will construct a transition matrix from the
#'   marginals if `x` doesn't have transitions, or return the relevant stored
#'   transition matrix if it does.
#'
#'   The format for a transition code is "T_\[from\]-\[to\]" where \[from\] and
#'   \[to\] are timesteps padded with zeros. Direction is important; "T_03-04"
#'   represents a transition backward in time.
#'
#'   The transition matrices are structured such that you multiply the matrix by
#'   the distribution to project either forward or backwards. If more than one
#'   distribution is projected at once they should be stored in columns of a
#'   matrix with dimensions([n_active()], [n_distr()]).
#'
#'   Given a marginal in which the cell \[i, j\] represents the probability of
#'   the bird being in state i in the prior timestep and state j in the next, to
#'   generate the forward transition matrix we divide each row of the marginal
#'   by its sum and then transpose. Backwards transitions matrices are generated
#'   by dividing each column by its sum, without transposing.
#'
#' @seealso [lookup_transitions()] will generate a list of the transitions
#'   needed to forecast or route between two points in time that can then be
#'   passed to this function. The internal function [transition_from_marginal()]
#'   does the calculations.
#' @export
get_transition <- function(x, transition){

   if(x$metadata$has_transitions){
     return(x$transition[[transition]])
   }

  if(x$metadata$has_marginals){
    ind <- x$marginals$index
    i <- which(ind$transition == transition)
    if(length(i) == 0){
      stop("There is no marginal for transition ", transition)
    }
    m <- x$marginals[[ind$marginal[i]]]
    direction <- ind$direction[i]
    return(transition_from_marginal(m, direction))
  }
  stop("The BirdFlow object should have either transitions or marginals")
}

#' convert a marginal into a transition matrix
#'
#' internal function to generate a transition matrix from a marginal
#'
#' @details this is called from [get_transition()]. If at some point we decide
#'   to store transitions rather than marginals it will also be called from
#'   [import_birdflow()].
#'
#' @param m a marginal
#' @param direction the desired transition direction, either "forward" or
#'   "backward"
#'
#' @return a transition matrix formulated such that you multiply the matrix by a
#'   distribution to project the distribution.  See [get_transition()] for more
#'   details.
transition_from_marginal <- function(m, direction){
  if(!direction %in% c("forward", "backward"))
    stop("Direction must be forward or backward")
  if(direction == "forward"){
    m <-  Matrix::t( m /Matrix::rowSums(m) )
  }
  if(direction == "backward"){
    m <-  apply(m, 2, function(x) x / sum(x))
  }
  m[is.na(m)] <- 0
  return(m)

}

