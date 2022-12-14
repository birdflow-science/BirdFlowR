% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_transition.R
\name{get_transition}
\alias{get_transition}
\title{extract a transition matrix from a BirdFlowR model}
\usage{
get_transition(x, obj)
}
\arguments{
\item{x}{transition code. E.g. "T_01-02"}

\item{obj}{a BirdFlow object}
}
\value{
a transition matrix
}
\description{
get_transition will return a transition matrix between the timesteps
indicated in the transition code \code{x} from a BirdFlow object, possibly
calculating them from marginals.
}
\details{
\code{get_transition} will construct a transition matrix from the marginals
if \code{obj}doesn't have transitions, or return the relevant stored transition
matrix if it does.

The nomenclature for a transition is "T_[from]-[to]" where [from] and
[to] are timesteps padded with zeros. Direction is important; "T_03-04"
represents a transition backward in time.

The transition matrices are structured such that you multiply the
matrix by the distribution to project either forward or backwards.
If more than one distribution is projected at once they should be
stored in columns of a matrix with dimensions(n_active, n_distributions).

Given a marginal in which the cell [i, j] represents the probability of the
bird being in state i in the prior timestep and state j in the next, to
generate the forward transition matrix we divide each row of the marginal by
its sum and then transpose. Backwards transitions matrices are generated by
dividing each column by its sum, without transposing.
}
\seealso{
\link{lookup_transitions} will generate a list of the transitions needed
to forecast or route between two points in time that can then be passed to
this function. The internal function \link{transition_from_marginal} does the
calculations.
}
