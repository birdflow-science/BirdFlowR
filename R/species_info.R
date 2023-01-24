#' lookup species information from a BirdFlow model
#'
#' BirdFlow model species metadata is from [ebirdst_runs][ebirdst::ebirdst_runs]
#'
#' @param x A `BirdFlow` model
#' @param what (optional) 'all' for all information (in a list) or the name of
#'   the information to be returned. See details for full option list.
#' @return if `what` is `all` a list of all the species information available;
#'  if omitted the common name of the species;  otherwise the element named by
#'   `what`.
#' @details `species()` returns data taken from [ebirdst::ebirdst_runs] and
#'  uses the same names. Descriptions copied from \pkg{ebirdst}):
#'  \describe{
#'   \item{species_code}{Six letter eBird code in eBird Taxonomy v2018}
#'   \item{scientific_name}{Scientific name from eBird Taxonomy v2018}
#'   \item{common_name}{English common name from eBird Taxonomy v2018}
#'   \item{breeding_quality}{Breeding season quality}
#'   \item{breeding_start}{Breeding season start date}
#'   \item{breeding_end}{Breeding season end date}
#'   \item{nonbreeding_quality}{Non-breeding season quality}
#'   \item{nonbreeding_start}{Non-breeding season start date}
#'   \item{nonbreeding_end}{Non-breeding season end date}
#'   \item{postbreeding_migration_quality}{Post-breeding season quality}
#'   \item{postbreeding_migration_start}{Post-breeding season start date}
#'   \item{postbreeding_migration_end}{Post-breeding season end date}
#'   \item{prebreeding_migration_quality}{Pre-breeding season quality}
#'   \item{prebreeding_migration_start}{Pre-breeding season start date}
#'   \item{prebreeding_migration_end}{Pre-breeding season end date}
#'}
#'
#'  'all' is a special case and returns the complete list.
#'
#'  For convenience the following are treated the same as the text in
#'  parenthesis:
#'    "code" ("species_code")
#'    "common" ("common name")
#'    "name" ("common_name")
#'    "scientific" ("scientific_name")
#'    "species" ("common_name")
#'
#' The 8 variables below are in [ebirdst::ebirdst_runs] but are dropped
#' from the BirdFlow model and thus can not be retrieved by `species()`.
#'
#' Four variables that track whether the full range is covered by eBird that
#' must be TRUE for a BirdFlow model to be fit:
#'  \describe{
#'   \item{postbreeding_migration_range_modeled}{Is the full range modeled?}
#'   \item{prebreeding_migration_range_modeled}{Is the full range modeled?}
#'   \item{nonbreeding_range_modeled}{Is the full range modeled?}
#'   \item{breeding_range_modeled}{Is the full range modeled?}
#'  }
#' `resident` is verified to be `FALSE` before the model is fit and three
#'  variables that relate to resident birds:
#'  \describe{
#'   \item{resident}{Classifies this species a resident or a migrant}
#'   \item{resident_quality}{Resident quality}
#'   \item{resident_start}{For resident species, the year-round start date}
#'   \item{resident_end}{For resident species, the year-round end date}
#'}
#' @export
species <- function(x, what){
  stopifnot(class(x) == "BirdFlow")
  if(missing(what))
    return(x$species$common_name)
  what <- tolower(what)

  if(what == "all")
    return(x$species)

  # Allow for non standard input
  what <- switch(what,
                 "common" = "common_name",
                 "name" = "common_name",
                 "species" = "common_name",
                 "scientific" = "scientific_name",
                 "code" = "species_code", # ebirdst used 'species_code',
                 what
                 )
  options <- c(names(x$species), "all")

  if(!what %in% options){
    stop("what should be one of: ", paste(options, collapse = ", "))
  }
  return(x$species[[what]])
}
