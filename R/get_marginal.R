#' Return a marginal matrix from a BirdFlowR model
#'
#'   Marginals in BirdFlow models are stored such that the cell \[i, j\]
#'   represents the probability of the bird being in state i in the prior
#'   timestep and state j in the next.  Thus the number of rows in the marginal
#'   equals the number of cells within the dynamic mask for the prior timestep
#'   and the columns count is equal to the included cells for the folling
#'   timestep.
#'
#' @param x A BirdFlow object
#' @param marginal A marginal code, e.g. "M_01-02"
#' @return A  marginal matrix
#'
#'
#' @seealso [lookup_transitions()] will generate a list of the transitions
#'   needed to predict or route between two points in time.  [get_transition()]
#'   will return a transition matrix - often calculated on the fly from a
#'   marginal.
#'
#' @export
get_marginal <- function(x, marginal, from, to) {
  if(!has_marginals(x))
    stop("x does not have marginals.")
  if(!marginal %in% names(x$marginals))
    stop(marginal, "is not a marginal in x.")
  return(bf$marginals[[marginal]])
}

