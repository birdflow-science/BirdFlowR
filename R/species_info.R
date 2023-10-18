#' retrieve species information from a BirdFlow model
#'
#' `species_info()` and `species()` return species data stored in a BirdFlow
#' model. They differ only in that `species()` returns the
#' common name by default so provides a succinct way to get the species name.
#'
#'
#' @param x `BirdFlow` model
#' @param what `"all"` for all information (in a list) or the name (see details)
#'   of the desired information. If `what` is omitted `species()` defaults
#'   to `"common_name"` and `species_info()` defaults to `"all"`.
#' @return the element named by `what`, unless `what` is `"all"` in which case a
#'   list of all the species information.  If `what` is omitted `species()` will
#'   return the common name of the species, and `species_info()` will return the
#'   complete list.
#' @details  The `what`
#'   argument takes the column names used in [ebirdst::ebirdst_runs] as input
#'   (descriptions from \pkg{ebirdst}):
#'| `species_code` | Six letter eBird code in eBird Taxonomy v2018 |
#'| --- | --- |
#'| `scientific_name` | Scientific name from eBird Taxonomy v2018 |
#'| `common_name` | English common name from eBird Taxonomy v2018 |
#'| `breeding_quality` | Breeding season quality |
#'| `breeding_start` | Breeding season start date |
#'| `breeding_end` | Breeding season end date |
#'| `nonbreeding_quality` | Non-breeding season quality |
#'| `nonbreeding_start` | Non-breeding season start date |
#'|`nonbreeding_end` | Non-breeding season end date |
#'|`postbreeding_migration_quality` | Post-breeding season quality |
#'|`postbreeding_migration_start` | Post-breeding season start date |
#'|`postbreeding_migration_end` | Post-breeding season end date |
#'|`prebreeding_migration_quality` | Pre-breeding season quality |
#'|`prebreeding_migration_start`| Pre-breeding season start date |
#'|`prebreeding_migration_end`| Pre-breeding season end date |
#'| | |
#'|`all` | returns the complete list |
#'
#'For convenience the following short versions are also accepted:
#'|    `code` | (`species_code`) |
#'| --- | --- |
#'|    `common` | (`common_name`) |
#'|    `name` | (`common_name`) |
#'|    `scientific`  | (`scientific_name`) |
#'|    `species` | (`common_name`) |
#'
#' @section{Dropped items}:
#' The 8 variables below are in [ebirdst::ebirdst_runs] but are dropped
#' from the BirdFlow model and thus can not be retrieved by
#' `species_info()`.
#'
#' Four variables that track whether the full range is covered by eBird that
#' must be TRUE for a BirdFlow model to be fit and are then dropped:
#'| `postbreeding_migration_range_modeled` | Is the full range modeled? |
#'| --- | --- |
#'| `prebreeding_migration_range_modeled` | Is the full range modeled? |
#'| `nonbreeding_range_modeled` | Is the full range modeled? |
#'| `breeding_range_modeled` | Is the full range modeled? |
#'
#' `resident` is verified to be `FALSE` before the model is fit. It and three
#'  related variables are then dropped:
#'| `resident` | Classifies this species a resident or a migrant |
#'| --- | --- |
#'| `resident_quality` | Resident quality |
#'| `resident_start` | For resident species, the year-round start date |
#'| `resident_end` | For resident species, the year-round end date |
#' @export
species_info <- function(x, what) {
  stopifnot(class(x) == "BirdFlow")

  if (missing(what))
    what <- "all"

  what <- tolower(what)

  if (what == "all")
    return(x$species)

  # Allow for non standard input
  what <- switch(what,
                 "common" = "common_name",
                 "name" = "common_name",
                 "species" = "common_name",
                 "scientific" = "scientific_name",
                 "code" = "species_code", # ebirdst used 'species_code',
                 what)
  options <- c(names(x$species), "all")

  if (!what %in% options) {
    stop("what should be one of: ", paste(options, collapse = ", "))
  }
  return(x$species[[what]])
}

#' @rdname species_info
#' @export
species <- function(x, what) {
  if (missing(what))
    what <- "common_name"
  return(species_info(x, what))
}
