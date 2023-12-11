#  This adds ebirdst_weeks as an internal dataset to the BirdFlowR package
#  so that it no longer needs to pull it from ebirdst.
#
#  Note: Internal data is stored in R/sysdata.rda and must all be created in
#  a single call.
#
#  ebirdst_weeks is from ebirdst version 2.2021.3.  It is added to BirdFlowR
#  for use when preprocessing ebirdst 2021 data (with the old ebird package)
#  without referencing the copy in that package as doing so - even conditioned
#  on version - breaks CRAN checks when checking against newer ebirdst package
#  versions that do not have it.

#  ebirdst_weeks.rds added to data-raw with Using ebirdst v 2.2021.3
#  with this: saveRDS(ebirdst::ebirdst_weeks, "data-raw/ebirdst_weeks.rds")
#  which never need to run again.

#  Add the data-raw/ebirdst_weeks.rds to R/sysdata.rda
ebirdst_weeks <- readRDS("./data-raw/ebirdst_weeks.rds")
usethis::use_data(ebirdst_weeks, internal = TRUE)

### ROxygen Comments below copied from ebirdst/R/data.R  v 2.2021.3
### They will not be rendered but kept here for reference.

#' eBird Status and Trends weeks
#'
#' eBird Status and Trends predictions are made for each of 52 weeks of the
#' year. This data frame provides the boundaries of the weeks.
#'
#' @format A data frame with 52 rows and 5 columns:
#' \describe{
#'   \item{week_number}{Integer week number from 1-52.}
#'   \item{date}{Date of the midpoint of the week.}
#'   \item{week_midpoint}{Date of the midpoint of the week expressed as a
#'         fraction of the year, i.e. a number from 0-1.}
#'   \item{week_start}{Date of the start of the week expressed as a fraction of
#'         the year, i.e. a number from 0-1.}
#'   \item{week_end}{Date of the end of the week expressed as a fraction of the
#'         year, i.e. a number from 0-1.}
#' }
"ebirdst_weeks"
