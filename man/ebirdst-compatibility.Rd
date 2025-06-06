% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ebirdst_compatibility.R
\name{ebirdst-compatibility}
\alias{ebirdst-compatibility}
\alias{ebirdst_pkg_ver}
\alias{ebirdst_ver_supported}
\alias{res_label}
\alias{date_to_week}
\alias{ebirdst_example_species}
\title{ebirdst version compatibility functions:}
\usage{
ebirdst_pkg_ver()

ebirdst_ver_supported(use = "package", throw_error = FALSE)

res_label(res)

date_to_week(dates, version = 2022)

ebirdst_example_species()
}
\arguments{
\item{use}{The particular use that is being checked against. \code{package} is
whether all package functions are supported fully.  \code{preprocess_species}
checks that function which has stricter requirements than most functions.
\code{lookup_species_metadata} checks that function which is called by
\code{Routes()}.}

\item{throw_error}{if \code{TRUE} an error will be thrown if \pkg{ebirdst} is not
supported by \code{use}.}

\item{res}{A resolution label. One of "lr", "mr", "hr", "27km", "9km", or
"3km}

\item{dates}{a vector of dates that can be processed by \code{as.POSIXlt()}}

\item{version}{A numeric (year) version of the eBird date scheme to use.
2021 for the older or 2022 for the newer; other values will be snapped to
the closest of those two. The output of
\code{ebirdst::ebirdst_version()$version_year} or
\code{get_metadata(bf, "ebird_version_year")} is appropriate.}
}
\value{
\code{ebirdst_pkg_ver()}: The installed \pkg{ebirdst} package version or
\code{NA} if none.

\code{ebirdst_ver_supported}: \code{TRUE} if the specified \code{use} is supported,
\code{FALSE} otherwise.

\code{res_label()}: resolution labels appropriate for installed version
of \pkg{ebirdst}.

\code{date_to_week()}: A vector of week numbers associated with \code{dates}

\code{ebirdst_example_species()}: The example species name for
\pkg{ebirdst}
}
\description{
Internal functions to facilitate working with both multiple versions of
of \pkg{ebirdst} despite the changes to the API between 2021 and 2022.
}
\section{\code{ebirdst_pkg_ver()}}{

\code{ebirdst_pkg_ver()} Look up the version of the currently installed
\pkg{ebirdst}.
}

\section{\code{ebirdst_ver_supported()}}{

\code{ebirdst_ver_supported()} Check whether the installed \pkg{ebirdst} is
supported by \pkg{BirdFlowR} for a particular use.
}

\section{\code{res_label()}}{

Convert resolution labels so they are appropriate for the
installed \pkg{ebirdst}

\pkg{ebirdst} 3.2022.0 switched from "lr", "mr", and "hr" to
"27km", "9km", and "3km" to indicate low, medium, and high resolution
versions of the raster data in function arguments.
\code{\link[=preprocess_species]{preprocess_species()}} uses the older two letter
versions but runs them through this function before calling \pkg{ebirdst}
functions.
}

\section{\code{date_to_week()}}{


This is a slightly modified copy of \code{ebirdst::date_to_st_week()} that
allows calculating weeks from dates without depending on \pkg{ebirdst}.
}

\section{\code{ebirdst_example_species()}}{

Lookup the example species name that is appropriate for the
installed \pkg{ebirdst}.  The example species changed
from \code{"example_data"} to \code{"yebsap-example"} in version 3.2022.0.
}

\keyword{internal}
