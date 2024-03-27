#' Return a marginal matrix from a BirdFlowR model
#'
#'   Marginals in BirdFlow models are stored such that the cell \[i, j\]
#'   represents the probability of the bird being in state i in the prior
#'   timestep and state j in the next.  Thus the number of rows in the marginal
#'   equals the number of cells within the dynamic mask for the prior timestep
#'   and the columns count is equal to the included cells for the following
#'   timestep.
#'
#' @param x A BirdFlow object
#' @param marginal A marginal code, e.g. "M_01-02"
#' @param from The first timestep associated with the marginal. Note marginals
#' are always forward so the second marginal will  be `from + 1` or `1` (when
#' `from` is the last timestep).
#' @return A  marginal matrix
#'
#' @seealso [lookup_transitions()] will generate a list of the transitions
#'   needed to predict or route between two points in time.  [get_transition()]
#'   will return a transition matrix - often calculated on the fly from a
#'   marginal.
#'
#' @export
get_marginal <- function(x, marginal = NULL, from = NULL) {
  if (!has_marginals(x))
    stop("x does not have marginals.")

  if (is.null(marginal)) {

    stopifnot(!is.null(from),
              is.numeric(from),
              length(from) == 1,
              from %in% seq_len(n_timesteps(x)))
    if (from == n_timesteps(x)) {
      if(!is_cyclical(x))
        stop("x isn't cyclical so there's no marginal with from = ", from)
      to <- 1
    } else {
      to <- from + 1
    }


    marginal <- paste0("M_",
                      pad_timestep(from, x), "-",
                      pad_timestep(to, x))
  }

  stopifnot(is.character(marginal), length(marginal) == 1, !is.na(marginal))

  if (!marginal %in% names(x$marginals))
    stop(marginal, "is not a marginal in x.")
  return(x$marginals[[marginal]])

}
