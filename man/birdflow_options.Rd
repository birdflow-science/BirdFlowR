% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdflow_options.R
\name{birdflow_options}
\alias{birdflow_options}
\title{Set and retrieve BirdFlowR options}
\usage{
birdflow_options(...)
}
\arguments{
\item{...}{One of:
(1) one or more named arguments where the name is a
an option and the value its new setting e.g. \code{verbose = FALSE} ;
(2) a single unnamed argument stating an option to retrieve e.g.
\code{"verbose"} with an option to retrieve.
(3) No arguments, indicating that all options and their current settings
should be returned in a list; or
(4) a single list argument with named items and their new values.}
}
\value{
If no arguments are used than all options will be returned as a list.
If there is a single, unnamed argument with a character value indicating an
option than the value of that option will be returned. Otherwise, the
arguments should indicate new option settings and nothing will be returned.
}
\description{
With no arguments all the BirdFlowR options will be returned as a list. Use a
single character value to retrieve the value of a single option. Use one or
more named arguments to set options.
}
\details{
\describe{
\item{time_format}{Indicates what time format to use to label dimensions of
distribution tables and layers of raster objects returned by \code{\link[=get_distr]{get_distr()}},
\code{\link[terra:rast]{terra::rast()}}, \code{\link[=rasterize_distr]{rasterize_distr()}}, and \code{\link[=predict]{predict()}}.  It does not affect
internally stored distribution column labels (which are always t1, t2, etc.).
Default is "month_day".

Valid values are: "timestep" which uses the timestep integer appended to "t"
e.g. "t1");  "date" which uses a date in the format year-month-day
(as numbers) e.g. "2022-11-23"; and "month_day" which uses the name of the
month followed by the day of the month e.g. "November 23."  }

\item{verbose}{Defaults to \code{TRUE} for printing of progress and information
about the process of functions.  Set to \code{FALSE} to turn off printing. }

\item{max_param_per_gpu_gb}{
Controls how many parameters can be fit by BirdFlowPy per gigabyte of
GPU Ram.  This is a conservative estimate based on empirical testing.  See
\code{\link[=preprocess_species]{preprocess_species()}}
}

\item{cache}{
The local directory to store downloaded model files.  Defaults to
\code{tools::R_user_dir("BirdFlowR", "data")}. This is the base cache directory
within which there will be one or more collection specific
directories, which in turn will hold BirdFlow model files and an index.
}
\item{collection_url}{
This is the base URL of a collection of model files and its associated
index.  The default is for the standard BirdFlowR model collection.
}
}
}
\examples{
bf <- BirdFlowModels::amewoo
birdflow_options() # print current settings
original_format <- birdflow_options("time_format")
birdflow_options(time_format = "date")
head(get_distr(bf, 1:3))
birdflow_options(time_format = original_format)

}
