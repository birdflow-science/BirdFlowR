% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_based_validation.R
\name{get_interval_based_validation_one_transition_pair}
\alias{get_interval_based_validation_one_transition_pair}
\title{Evaluate model performance using intervals (movement data)}
\usage{
get_interval_based_validation_one_transition_pair(
  birdflow_interval_row,
  bf,
  gcd,
  st_dists
)
}
\arguments{
\item{birdflow_interval_row}{A row of data in the \code{BirdFlowIntervals} object}

\item{bf}{BirdFlow model}

\item{gcd}{Matrix of great circle distance}

\item{st_dists}{Matrix of S&T distribution with weeks as columns,
location as rows, probability as values.}
}
\value{
A named vector with distance metrics
}
\description{
Calculate the interval based validation, including predictive
distance metric, log likelihood, and log likelihood baseline
For distance metrics: What is the distance and probability that BirdFlow
model prediction beats the naive S&T probability
distribution-based prediction?
Get the interval based validation metrics for one transition pair
}
