% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Routes-internal.R
\name{add_stay_id_with_varied_intervals}
\alias{add_stay_id_with_varied_intervals}
\title{Add Stay IDs with Temporal Thresholds}
\usage{
add_stay_id_with_varied_intervals(
  df,
  date_col = "date",
  timediff_unit = "days"
)
}
\arguments{
\item{df}{A data frame with spatial and temporal data.}

\item{date_col}{The name of the column containing the
date information. Defaults to \code{"date"}.}

\item{timediff_unit}{The unit of \code{stay_len}.}
}
\value{
A data frame with \code{stay_id} and \code{stay_len} columns added.
}
\description{
Adds stay IDs to a data frame,
considering changes in spatial indices.
Should only be applied on a single route, not multiple.
Using \code{add_stay_id_with_varied_intervals()}, rather than \code{add_stay_id()}:
It takes \code{date} as input so account for varying intervals,
if the data is not sampled in the same frequency.
}
\examples{
routes <- data.frame(list(
  route_id = c(1, 1, 1, 2, 2, 3, 3, 3),
  i = as.integer(c(1, 1, 2, 2, 3, 4, 4, 5)), # Spatial index
  date = as.Date(c(
    "2010-01-01", "2010-01-02", "2010-01-05", "2010-01-06",
    "2010-01-10", "2010-01-15", "2010-01-16", "2010-01-20"
  )) # Time steps with varying intervals
))
df_with_varied_stay_ids <-
 add_stay_id_with_varied_intervals(routes, "date", "days")
}
