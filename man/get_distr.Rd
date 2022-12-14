% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_distr.R
\name{get_distr}
\alias{get_distr}
\title{extract distributions from BirdFlow models}
\usage{
get_distr(which, obj, from_marginals = FALSE)
}
\arguments{
\item{which}{indicates which distributions to return. Can be one or more
integers indicating timesteps; character dates in the format
year-month-day e.g. \code{"2019-02-25"}; \code{\link[base:Dates]{Date}} objects; or
\code{"all"} which will return distributions for all timesteps.}

\item{obj}{a BirdFlow model}

\item{from_marginals}{if TRUE and \code{obj} has marginals the distribution will
be from the marginals even if \code{obj} also has distributions.}
}
\value{
either a vector with a distribution for a single timestep or a matrix
with a column for each distribution.
}
\description{
\code{get_distr} will return one or more distributions in their compact form.
A single distribution will be returned as a vector; if multiple they will be
columns in a matrix.
}
\details{
If the BirdFlow object has stored distributions they will be the training
distributions and will be returned by default unless \code{from_marginals = TRUE}
in which case distributions calculated from the marginal will be returned.

The training distributions and the distributions calculated from the
marginal are very similar.
}
\seealso{
Distributions can be passed to \link{forecast} or converted to rasters
with \link{expand_distr} or converted to \link[terra:rast]{SpatRaster} with
\link{rasterize_distr}.  \link{sample_distr} will convert one cell to 1 and the rest
to 0 probabilistically based on the densities in the distribution.
}
